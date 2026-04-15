{- ORMOLU_DISABLE -}
module Test.LetLayout where {

  -- | Uses implicit layout in a let expression.
  letExample :: Int;
  letExample = let
      x = 1
      y = 2
    in x
}
