{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fplugin-opt NoRecursion:ignore-decls:collectLayoutViolations_,splitMap #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- A GHC plugin that disables the use of the "layout rule" for significant
-- whitespace in Haskell code. Any module compiled with this plugin must use
-- explicit braces @{ }@ and semicolons @;@ instead of relying on indentation.
--
-- = Usage
--
-- Add @-fplugin NoLayout@ to your GHC options.
--
-- To allow layout in specific syntactic constructs, use e.g.:
--
-- > -fplugin-opt NoLayout:allowIn=HsModule,ClassDecl
--
-- Recognized construct names: @HsModule@, @ClassDecl@, @HsDo@, @HsValBinds@.
--
-- @since 0.0.1
module NoLayout
  ( plugin,
    LayoutContext (..),
    LayoutViolation (..),
    collectLayoutViolations,
    hasLayoutViolations,
  )
where

import safe "base" Control.Applicative (empty, pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Exception (ErrorCall (ErrorCall), throwIO)
import safe "base" Data.Bool (Bool (False, True), not, otherwise, (&&))
import safe "base" Data.Char (Char)
import safe "base" Data.Data (Data, gmapQ)
import safe "base" Data.Eq (Eq, (/=), (==))
import safe "base" Data.Foldable
  ( concat,
    concatMap,
    notElem,
    null,
    toList,
    traverse_,
  )
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.Int (Int)
import safe "base" Data.Kind (Type)
import safe "base" Data.List (filter, span, stripPrefix)
import safe "base" Data.List.NonEmpty (nonEmpty)
import safe "base" Data.Maybe
  ( Maybe (Just),
    catMaybes,
    fromMaybe,
    isJust,
    listToMaybe,
    mapMaybe,
    maybe,
  )
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Typeable (cast)
import safe "base" System.IO (IO)
import safe "base" Text.Show (Show)
import "ghc" GHC.Hs (HsLocalBindsLR, HsParsedModule, hpm_module)
import "ghc" GHC.Hs.Extension (GhcPs)
import "ghc" GHC.Parser.Annotation (AnnList, EpAnn (EpAnn), al_anchor)
import "ghc" GHC.Plugins
  ( CommandLineOption,
    Hsc,
    ModSummary,
    Plugin,
    defaultPlugin,
    liftIO,
    parsedResultAction,
    pluginRecompile,
    purePlugin,
  )
import "ghc" GHC.Types.SrcLoc (SrcSpan, noSrcSpan)
import "ghc" GHC.Utils.Outputable
  ( SDoc,
    colon,
    defaultSDocContext,
    int,
    nest,
    ppr,
    renderWithContext,
    text,
    vcat,
    ($$),
    (<+>),
  )
import "ghc" GHC.Utils.Outputable qualified as Out
import "ghc" Language.Haskell.Syntax.Expr (HsExpr (HsDo))
#if MIN_VERSION_ghc(9, 12, 0)
import safe "base" Data.Bool ((||))
import "ghc" GHC.Parser.Annotation
  ( AnnListBrackets (ListNone),
    EpaLocation,
    EpToken,
    al_brackets,
  )
#elif MIN_VERSION_ghc(9, 10, 0)
import safe "base" Data.Maybe (isNothing)
import "ghc" GHC.Parser.Annotation  (al_open)
#else
import safe "base" Data.Maybe (isNothing)
import "ghc" GHC.Parser.Annotation
  ( SrcSpanAnn' (SrcSpanAnn),
    SrcSpanAnnL,
    al_open,
  )
#endif
#if MIN_VERSION_ghc(9, 10, 0)
import "ghc" GHC.Hs (XModulePs)
import "ghc" GHC.Parser.Annotation (EpLayout (EpVirtualBraces))
#elif MIN_VERSION_ghc(9, 6, 0)
import "ghc" GHC.Hs (XModulePs)
import "ghc" Language.Haskell.Syntax.Concrete (LayoutInfo (VirtualBraces))
#else
import "ghc" GHC.Hs (HsModule)
import "ghc" GHC.Types.SrcLoc (LayoutInfo (VirtualBraces))
#endif
#if MIN_VERSION_ghc(9, 4, 0)
import "ghc" GHC.Driver.Plugins (ParsedResult, parsedResultModule)
#endif

-- | The entrypoint for the "NoLayout" plugin.
--
-- @since 0.0.1
plugin :: Plugin
plugin =
  defaultPlugin
    { pluginRecompile = purePlugin,
      parsedResultAction = noLayoutAction
    }

-- | The syntactic context in which a layout violation was found.
--
-- @since 0.0.1
type LayoutContext :: Type
data LayoutContext
  = -- | The top-level module @where@ clause.
    --
    -- @since 0.0.1
    HsModuleContext
  | -- | A @class@ declaration body.
    --
    -- @since 0.0.1
    ClassDeclContext
  | -- | A @do@ block (including @mdo@).
    --
    -- @since 0.0.1
    HsDoContext
  | -- | A binding group (@where@ clause or @let@ bindings).
    --
    -- @since 0.0.1
    HsValBindsContext
  deriving (Eq, Show)

-- | A record of a layout rule violation found in the parsed AST.
--
-- @since 0.0.1
type LayoutViolation :: Type
data LayoutViolation = LayoutViolation
  { -- | The nearest enclosing source location.
    --
    -- @since 0.0.1
    lvSrcSpan :: SrcSpan,
    -- | The layout column from 'EpVirtualBraces', or @-1@ for
    --   'AnnList'-detected layout.
    --
    -- @since 0.0.1
    lvColumn :: Int,
    -- | The syntactic context of the violation.
    --
    -- @since 0.0.1
    lvContext :: LayoutContext
  }

-- | Collect all layout violations in a 'Data' structure: both
--   'EpVirtualBraces' occurrences and 'AnnList' nodes that indicate
--   implicit layout (present anchor, absent opening brace).
--
-- @since 0.0.1
collectLayoutViolations :: (Data a) => a -> [LayoutViolation]
collectLayoutViolations = collectLayoutViolations_ noSrcSpan empty empty

collectLayoutViolations_ ::
  (Data a) =>
  SrcSpan ->
  Maybe LayoutContext ->
  Maybe LayoutContext ->
  a ->
  [LayoutViolation]
collectLayoutViolations_ currentSpan epLayoutCtx annListCtx x =
  -- 1. Check for virtual braces (module-level where, class decl body).
  maybe
    ( -- 2. Check for AnnList-based layout (do, where, let).
      annListViolation span' annListCtx' x
        -- 3. Recurse into children.
        <> concat
          (gmapQ (collectLayoutViolations_ span' epLayoutCtx' annListCtx') x)
    )
    ( maybe
        empty
        ( \col ->
            pure . LayoutViolation span' col $
              fromMaybe ClassDeclContext epLayoutCtx
        )
        . virtualColumn
    )
    $ castToLayoutType x
  where
    span' = fromMaybe (fromMaybe currentSpan $ firstChildSrcSpan x) $ cast x
    -- Update the context: detect when we're inside a module or class decl.
    -- Reset to Nothing once set, so it only applies to the immediate children.
    epLayoutCtx'
      | isModuleExt x = pure HsModuleContext
      | isJust epLayoutCtx = empty -- reset after one level
      | otherwise = epLayoutCtx
    -- Update the AnnList context based on the current node type.
    annListCtx' = detectAnnListContext x annListCtx

#if MIN_VERSION_ghc(9, 10, 0)
virtualColumn :: EpLayout -> Maybe Int
virtualColumn = \case
  EpVirtualBraces col -> pure col
  _ -> empty
#else
#if MIN_VERSION_ghc(9, 6, 0)
virtualColumn :: LayoutInfo GhcPs -> Maybe Int
#else
virtualColumn :: LayoutInfo -> Maybe Int
#endif
virtualColumn = \case
  VirtualBraces col -> pure col
  _ -> empty
#endif

-- | Cast a node to the layout type used by the current GHC version.
--   GHC 9.10+ uses 'EpLayout'; 9.6–9.8 use @LayoutInfo GhcPs@;
--   9.2–9.4 use @LayoutInfo@ (unparameterized).
#if MIN_VERSION_ghc(9, 10, 0)
castToLayoutType :: (Data a) => a -> Maybe EpLayout
#elif MIN_VERSION_ghc(9, 6, 0)
castToLayoutType :: (Data a) => a -> Maybe (LayoutInfo GhcPs)
#else
castToLayoutType :: (Data a) => a -> Maybe LayoutInfo
#endif
castToLayoutType = cast

-- | Check if a node is the module extension type that contains layout info.
--   GHC 9.6+ uses 'XModulePs'; 9.2–9.4 use 'HsModule' directly.
isModuleExt :: (Data a) => a -> Bool
#if MIN_VERSION_ghc(9, 6, 0)
isModuleExt x = isJust (cast x :: Maybe XModulePs)
#else
isModuleExt x = isJust (cast x :: Maybe HsModule)
#endif

-- | Detect the 'LayoutContext' for 'AnnList' layout based on the current
--   AST node. Returns the context that should apply to any 'AnnList' found
--   as a child of this node.
detectAnnListContext ::
  (Data a) => a -> Maybe LayoutContext -> Maybe LayoutContext
detectAnnListContext x prev
  -- HsDo carries AnnList directly; children see HsDoContext
  | isHsDo x = pure HsDoContext
  -- HsValBinds (where/let bindings) carries EpAnn AnnList
  | isHsValBinds x = pure HsValBindsContext
  | otherwise = prev

-- | Check if a node is an @HsDo@ expression.
isHsDo :: (Data a) => a -> Bool
isHsDo x = case cast x :: Maybe (HsExpr GhcPs) of
  Just (HsDo {}) -> True
  _ -> False

-- | Check if a node is an @HsValBinds@ local bindings.
isHsValBinds :: (Data a) => a -> Bool
isHsValBinds x = isJust (cast x :: Maybe (HsLocalBindsLR GhcPs GhcPs))

-- | If this node indicates implicit layout, produce a violation.
--   This detects layout in @do@ blocks, @where@ clauses, @let@ bindings, etc.
--   Only produces a violation when we're inside a known layout context
--   (set by 'detectAnnListContext').
annListViolation ::
  (Data a) => SrcSpan -> Maybe LayoutContext -> a -> [LayoutViolation]
annListViolation span' annListCtx x =
  case annListCtx of
    Just ctx
      | annListUsesLayout ctx x ->
          pure $ LayoutViolation span' (-1) ctx
    _ -> empty

-- | Check whether this node is an @AnnList@-bearing structure that uses layout.
--   Takes the current 'LayoutContext' to select the right detection strategy.
annListUsesLayout :: (Data a) => LayoutContext -> a -> Bool
#if MIN_VERSION_ghc(9, 12, 0)
-- In 9.12+, AnnList is parameterized. We try the two known concrete types.
annListUsesLayout _ x =
  checkEpAnnList (cast x :: Maybe (EpAnn (AnnList EpaLocation)))
    || checkEpAnnList (cast x :: Maybe (EpAnn (AnnList (EpToken "where"))))
  where
    checkEpAnnList = maybe False $ \(EpAnn _ al _) ->
      isJust (al_anchor al) && al_brackets al == ListNone
#elif MIN_VERSION_ghc(9, 10, 0)
-- In 9.10, XDo GhcPs = AnnList (bare), so EpAnn AnnList only appears
-- in stmts wrappers and val bindings — no false positives from HsDo.
annListUsesLayout _ x =
  case cast x :: Maybe (EpAnn AnnList) of
    Just (EpAnn _ al _) -> isJust (al_anchor al) && isNothing (al_open al)
    _ -> False
#else
-- In 9.4–9.8, XDo GhcPs = EpAnn AnnList, so for HsDo context we cast to
-- SrcSpanAnnL (stmts wrapper) instead of bare EpAnn AnnList to avoid false
-- positives from HsDo's keyword annotation.
-- For HsValBinds context, bare EpAnn AnnList is safe.
-- Pattern match on EpAnn constructor to handle EpAnnNotUsed (9.4–9.8).
annListUsesLayout HsDoContext x =
  case cast x :: Maybe SrcSpanAnnL of
    Just (SrcSpanAnn (EpAnn _ al _) _) ->
      isJust (al_anchor al) && isNothing (al_open al)
    _ -> False
annListUsesLayout _ x =
  case cast x :: Maybe SrcSpanAnnL of
    Just (SrcSpanAnn (EpAnn _ al _) _) ->
      isJust (al_anchor al) && isNothing (al_open al)
    _ -> checkEpAnnList x
  where
    checkEpAnnList y = case cast y :: Maybe (EpAnn AnnList) of
      Just (EpAnn _ al _) -> isJust (al_anchor al) && isNothing (al_open al)
      _ -> False
#endif

-- | Find the first immediate child that is a 'SrcSpan'.
firstChildSrcSpan :: (Data a) => a -> Maybe SrcSpan
firstChildSrcSpan = listToMaybe . catMaybes . gmapQ cast

-- | Check whether any layout violations exist in a 'Data' structure.
--
-- @since 0.0.1
hasLayoutViolations :: (Data a) => a -> Bool
hasLayoutViolations = not . null . collectLayoutViolations

-- | Parse a comma-separated @allowIn=…@ option value.
parseAllowIn :: String -> [LayoutContext]
parseAllowIn = catMaybes . splitMap parseContextName ','

-- | Recognize a construct name.
parseContextName :: String -> Maybe LayoutContext
parseContextName = \case
  "HsModule" -> pure HsModuleContext
  "ClassDecl" -> pure ClassDeclContext
  "HsDo" -> pure HsDoContext
  "HsValBinds" -> pure HsValBindsContext
  _ -> empty

splitMap :: (String -> a) -> Char -> String -> [a]
splitMap _ _ [] = []
splitMap f sep s =
  let (chunk, rest) = span (/= sep) s
   in f chunk : splitMap f sep rest

-- | Parse plugin options, returning the list of allowed contexts.
parseOpts :: [CommandLineOption] -> [LayoutContext]
parseOpts = concatMap parseAllowIn . mapMaybe (stripPrefix "allowIn=")

contextLabel :: LayoutContext -> String
contextLabel = \case
  HsModuleContext -> "module"
  ClassDeclContext -> "class declaration"
  HsDoContext -> "do block"
  HsValBindsContext -> "bindings"

formatViolation :: LayoutViolation -> SDoc
formatViolation (LayoutViolation srcSpan col ctx) =
  (ppr srcSpan Out.<> colon <+> text "error:")
    $$ nest
      4
      ( ( text "Layout rule used in"
            <+> text (contextLabel ctx)
            Out.<> columnInfo col
            Out.<> text "."
        )
          $$ text "Use explicit braces ‘{ }’ and semicolons ‘;’ instead."
      )
  where
    columnInfo c
      | c == (-1) = text ""
      | otherwise = text " (column" <+> int c Out.<> text ")"

checkForLayout :: [LayoutContext] -> HsParsedModule -> IO ()
checkForLayout allowed =
  traverse_
    ( throwIO
        . ErrorCall
        . renderWithContext defaultSDocContext
        . vcat
        . toList
        . fmap formatViolation
    )
    . nonEmpty
    . filter (\v -> lvContext v `notElem` allowed)
    . collectLayoutViolations
    . hpm_module

#if MIN_VERSION_ghc(9, 4, 0)
extractHsParsedModule :: ParsedResult -> HsParsedModule
extractHsParsedModule = parsedResultModule

noLayoutAction ::
  [CommandLineOption] ->
  ModSummary ->
  ParsedResult ->
  Hsc ParsedResult
#else
extractHsParsedModule :: HsParsedModule -> HsParsedModule
extractHsParsedModule hpm = hpm

noLayoutAction ::
  [CommandLineOption] ->
  ModSummary ->
  HsParsedModule ->
  Hsc HsParsedModule
#endif
noLayoutAction opts _modSummary result = liftIO do
  checkForLayout (parseOpts opts) $ extractHsParsedModule result
  pure result
