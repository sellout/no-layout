module Test.ImplicitLayout where

import Data.Function (($))

-- | A function using implicit layout for let-in.
example :: a -> a
example x = let y = x in y

-- | A function using implicit layout for do-notation.
doExample :: IO ()
doExample = do
  pure ()
