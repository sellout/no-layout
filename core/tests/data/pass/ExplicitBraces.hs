{- ORMOLU_DISABLE -}
module Test.ExplicitBraces where {

  import Data.Function (($));

  -- | A function using explicit let-in braces.
  example :: a -> a;
  example x = let { y = x } in y;

  -- | A function using explicit do-notation braces.
  doExample :: IO ();
  doExample = do { pure () }
}
