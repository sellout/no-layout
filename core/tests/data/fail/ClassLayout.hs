{- ORMOLU_DISABLE -}
module Test.ClassLayout where {

  -- | Uses implicit layout in a class declaration.
  class MyClass a where
    myMethod :: a -> a
}
