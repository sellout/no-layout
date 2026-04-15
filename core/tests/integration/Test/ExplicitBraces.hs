{- ORMOLU_DISABLE -}
{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- A test module using only explicit braces. This module is compiled with the
-- "NoLayout" plugin, so it will fail to compile if any layout-inserted braces
-- are detected.
module Test.ExplicitBraces (example, doExample) where {

  import "base" Control.Applicative (pure);
  import "base" System.IO (IO);

  -- | A function using explicit let-in braces.
  --
  -- @since 0.0.1
  example :: a -> a;
  example x = let { y = x } in y;

  -- | A function using explicit do-notation braces.
  --
  -- @since 0.0.1
  doExample :: IO ();
  doExample = do { pure () }
}
