{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.Holes
import Language.Haskell.TH

-- \x -> x
i :: a -> a
i = $(hole [| I :: a -> a |])

-- \x y -> x y y
w :: (a -> a -> b) -> a -> b
w = $(hole [| W :: (a -> a -> b) -> a -> b |])

-- \x y z -> x (y z)
b :: (b -> c) -> (a -> b) -> (a -> c)
b = $(hole [| B :: (b -> c) -> (a -> b) -> (a -> c) |])

-- \x y z -> x z y
c :: (a -> b -> c) -> (b -> a -> c)
c = $(hole [| C :: (a -> b -> c) -> (b -> a -> c) |])

-- \u x y -> x (u x y)
sp :: ((b -> c) -> a -> b) -> (b -> c) -> a -> c
sp = $(hole [| SPlus :: ((b -> c) -> a -> b) -> (b -> c) -> a -> c |])

-- \x y z w -> x ((y w) (z w))
f1 :: (b -> c) -> (d -> a -> b) -> (d -> a) -> (d -> c)
f1 = $(hole [| _ :: (b -> c) -> (d -> a -> b) -> (d -> a) -> d -> c |])

-- Some types from Prelude are already in the context, so the values can be inferred.
t1 :: String
t1 = $(hole [| _ :: String |])

t2 :: String -> Int
t2 = $(hole [| _ :: String -> Int |])

-- Example with custom context
t3 :: Maybe Int
t3 = $(holeWith
       -- context = [Just 0 :: Maybe Int]
       [([| Just 0 |], AppT (ConT ''Maybe) (ConT ''Int))]
       [| mi :: Maybe Int |])
