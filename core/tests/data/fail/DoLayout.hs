{- ORMOLU_DISABLE -}
module Test.DoLayout where {

  import System.IO (putStrLn, readLn);

  -- | Uses implicit layout in a do-block.
  doExample :: IO ();
  doExample = do
    x <- readLn
    putStrLn x
}
