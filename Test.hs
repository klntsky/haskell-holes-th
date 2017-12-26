{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.Holes
import Language.Haskell.TH

-- \x -> x
i :: a -> a
i = $(hole [| i :: a -> a |])

-- \x y -> x y y
w :: (a -> a -> b) -> a -> b
w = $(hole [| w :: (a -> a -> b) -> a -> b |])

-- \x y z -> x (y z)
b :: (b -> c) -> (a -> b) -> (a -> c)
b = $(hole [| b :: (b -> c) -> (a -> b) -> (a -> c) |])

-- \x y z -> x z y
c :: (a -> b -> c) -> (b -> a -> c)
c = $(hole [| c :: (a -> b -> c) -> (b -> a -> c) |])

-- \u x y -> x (u x y)
sp :: ((b -> c) -> a -> b) -> (b -> c) -> a -> c
sp = $(hole [| sp :: ((b -> c) -> a -> b) -> (b -> c) -> a -> c |])

-- \x y z w -> x ((y w) (z w))
f1 :: (b -> c) -> (d -> a -> b) -> (d -> a) -> (d -> c)
f1 = $(hole [| f1 :: (b -> c) -> (d -> a -> b) -> (d -> a) -> d -> c |])

-- Some types from Prelude are already in the context, so the values can be inferred.
f2 :: String
f2 = $(hole [| f2 :: String |])

f3 :: String -> Int
f3 = $(hole [| f3 :: String -> Int |])

-- Example with custom context
f4 :: Maybe Int
f4 = $(holeWith
       -- context = [Just 0 :: Maybe Int]
       [([| Just 0 |], AppT (ConT ''Maybe) (ConT ''Int))]
       [| f4 :: Maybe Int |])
