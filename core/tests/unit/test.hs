{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -fplugin-opt NoRecursion:ignore-decls:assertAllRealSpans #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Unit tests for the "NoLayout" plugin. Uses the GHC API to parse Haskell
-- source files and checks whether 'collectLayoutViolations' correctly detects
-- layout violations with the expected contexts.
module Main (main) where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Monad (unless)
import safe "base" Control.Monad.IO.Class (liftIO)
import safe "base" Data.Bool (Bool (False, True), not)
import safe "base" Data.Eq ((==))
import safe "base" Data.Foldable (any, null)
import safe "base" Data.Function (($))
import safe "base" Data.List (length)
import safe "base" Data.Maybe (Maybe (Just, Nothing))
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" System.Exit (exitFailure)
import safe "base" System.IO (IO, hPutStrLn, putStrLn, stderr)
import safe "base" Text.Show (show)
import "ghc" GHC
  ( LoadHowMuch (LoadAllTargets),
    getModuleGraph,
    getSessionDynFlags,
    guessTarget,
    load,
    mgModSummaries,
    parseModule,
    pm_parsed_source,
    runGhc,
    setSessionDynFlags,
    setTargets,
  )
import "ghc" GHC.Types.SrcLoc (SrcSpan (RealSrcSpan))
import "ghc-paths" GHC.Paths (libdir)
import safe "no-layout" NoLayout
  ( LayoutContext
      ( ClassDeclContext,
        HsDoContext,
        HsModuleContext,
        HsValBindsContext
      ),
    LayoutViolation,
    collectLayoutViolations,
    lvContext,
    lvSrcSpan,
  )

-- | Parse a Haskell source file and collect layout violations.
parseAndCollect :: String -> IO [LayoutViolation]
parseAndCollect path = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags dflags
  target <- guessTarget path Nothing Nothing
  setTargets [target]
  _ <- load LoadAllTargets
  modGraph <- getModuleGraph
  case mgModSummaries modGraph of
    [modSumm] -> do
      pm <- parseModule modSumm
      pure (collectLayoutViolations (pm_parsed_source pm))
    _ -> do
      liftIO $ hPutStrLn stderr ("Expected exactly one module in " <> path)
      liftIO exitFailure

-- | Assert a condition, printing a message and exiting on failure.
assert :: Bool -> String -> IO ()
assert condition msg = unless condition $ do
  hPutStrLn stderr ("FAIL: " <> msg)
  exitFailure

-- | Check that a 'SrcSpan' is a real location.
isRealSrcSpan :: SrcSpan -> Bool
isRealSrcSpan (RealSrcSpan _ _) = True
isRealSrcSpan _ = False

-- | Assert that all violations have real source spans.
assertAllRealSpans :: [LayoutViolation] -> IO ()
assertAllRealSpans [] = pure ()
assertAllRealSpans (v : vs) = do
  assert
    (isRealSrcSpan (lvSrcSpan v))
    "Expected a real SrcSpan, got <no location info>"
  assertAllRealSpans vs

-- | Assert that at least one violation has the given context.
assertHasContext :: LayoutContext -> [LayoutViolation] -> String -> IO ()
assertHasContext ctx vs label =
  assert
    (any (\v -> lvContext v == ctx) vs)
    (label <> ": expected a " <> show ctx <> " violation")

-- | The test entry point.
--
-- @since 0.0.1
main :: IO ()
main = do
  -- Explicit braces: should have no violations.
  putStrLn "Testing explicit braces (should have no violations)..."
  passViolations <- parseAndCollect "tests/data/pass/ExplicitBraces.hs"
  assert
    (null passViolations)
    ( "ExplicitBraces.hs: expected 0 violations, got "
        <> show (length passViolations)
    )
  putStrLn "  OK: 0 violations"

  -- Module-level implicit layout.
  putStrLn "Testing module-level layout..."
  moduleVs <- parseAndCollect "tests/data/fail/ImplicitLayout.hs"
  assert (not (null moduleVs)) "ImplicitLayout.hs: expected violations"
  assertAllRealSpans moduleVs
  assertHasContext HsModuleContext moduleVs "ImplicitLayout.hs"
  putStrLn ("  OK: " <> show (length moduleVs) <> " violation(s)")

  -- Do-block implicit layout.
  putStrLn "Testing do-block layout..."
  doVs <- parseAndCollect "tests/data/fail/DoLayout.hs"
  assert (not (null doVs)) "DoLayout.hs: expected violations"
  assertHasContext HsDoContext doVs "DoLayout.hs"
  putStrLn ("  OK: " <> show (length doVs) <> " violation(s)")

  -- Where-clause implicit layout.
  putStrLn "Testing where-clause layout..."
  whereVs <- parseAndCollect "tests/data/fail/WhereLayout.hs"
  assert (not (null whereVs)) "WhereLayout.hs: expected violations"
  assertHasContext HsValBindsContext whereVs "WhereLayout.hs"
  putStrLn ("  OK: " <> show (length whereVs) <> " violation(s)")

  -- Let-expression implicit layout.
  putStrLn "Testing let-expression layout..."
  letVs <- parseAndCollect "tests/data/fail/LetLayout.hs"
  assert (not (null letVs)) "LetLayout.hs: expected violations"
  assertHasContext HsValBindsContext letVs "LetLayout.hs"
  putStrLn ("  OK: " <> show (length letVs) <> " violation(s)")

  -- Class declaration implicit layout.
  putStrLn "Testing class declaration layout..."
  classVs <- parseAndCollect "tests/data/fail/ClassLayout.hs"
  assert (not (null classVs)) "ClassLayout.hs: expected violations"
  assertHasContext ClassDeclContext classVs "ClassLayout.hs"
  putStrLn ("  OK: " <> show (length classVs) <> " violation(s)")

  putStrLn "All unit tests passed."
