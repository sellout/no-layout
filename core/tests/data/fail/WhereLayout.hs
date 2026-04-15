{- ORMOLU_DISABLE -}
module Test.WhereLayout where {

  -- | Uses implicit layout in a where clause.
  whereExample :: Int;
  whereExample = y
    where
      y = 1
}
