{- ORMOLU_DISABLE -}
{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Integration tests for the "NoLayout" plugin. The test modules in this
-- suite are compiled with the "NoLayout" plugin enabled. If they compile
-- successfully, it confirms the plugin accepts code using explicit braces.
module Main (main) where {

  import safe "base" Control.Applicative (pure);
  import safe "base" System.IO (IO);
  import "no-layout" NoLayout ();

  -- | The test entry point.
  --
  -- @since 0.0.1
  main :: IO ();
  main = pure ()
}
