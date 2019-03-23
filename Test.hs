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

-- Proving that (->) is an instance of Profunctor
dimap :: (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
dimap = $(hole [| dimap :: (a -> b) -> (c -> d) -> (b -> c) -> (a -> d) |])

-- Proving that (->) is an instance of Closed
closed :: (a -> b) -> (x -> a) -> (x -> b)
closed = $(hole [| closed :: (a -> b) -> (x -> a) -> (x -> b) |])

-- Proving that (->) is an instance of Strong
first :: (a -> b) -> (a, c) -> (b, c)
first = $(hole [| first :: (a -> b) -> (a, c) -> (b, c) |])

second :: (b -> c) -> (a, b) -> (a, c)
second = $(hole [| second :: (b -> c) -> (a, b) -> (a, c) |])

-- Simple tests

prod1 :: a -> (a, a)
prod1 = $(hole [| prod1 :: a -> (a, a) |])

prod2 :: a -> b -> (a, a)
prod2 = $(hole [| prod2 :: a -> b -> (a, a) |])

prod3 :: a -> b -> (a, b)
prod3 = $(hole [| prod3 :: a -> b -> (a, b) |])

prod4 :: a -> b -> c -> (a, c, b, (a, c))
prod4 = $(hole [| prod4 :: a -> b -> c -> (a, c, b, (a, c)) |])

prod5 :: a -> b -> c -> (a -> c, d -> c, b -> (a, c, b -> a))
prod5 = $(hole [| prod5 :: a -> b -> c -> (a -> c, d -> c, b -> (a, c, b -> a)) |])

proj1 :: (a, a) -> a
proj1 = $(hole [| proj1 :: (a, a) -> a |])

proj2 :: (a, b) -> a
proj2 = $(hole [| proj2 :: (a, b) -> a |])

proj3 :: (a, b) -> b
proj3 = $(hole [| proj3 :: (a, b) -> b |])

swap :: (a, b) -> (b, a)
swap = $(hole [| swap :: (a, b) -> (b, a) |])

rebalance :: (a, (b, c)) -> (a, (b, c))
rebalance = $(hole [| rebalance :: (a, (b, c)) -> (a, (b, c)) |])

unit :: ()
unit = $(hole [| unit :: () |])
