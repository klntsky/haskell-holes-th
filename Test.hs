{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.Holes

-- \x -> x
i :: a -> a
i = $(hole ('a' --> 'a'))

-- \x y -> x y y
w :: (a -> a -> b) -> a -> b
w = $(hole $ ('a' --> 'a' --> 'b') --> ('a' --> 'b'))

-- \x y z -> x (y z)
b :: (b -> c) -> (a -> b) -> (a -> c)
b = $(hole $ ('b' --> 'c') --> ('a' --> 'b') --> ('a' --> 'c'))

-- \x y z -> x z y
c :: (a -> b -> c) -> (b -> a -> c)
c = $(hole $ ('a' --> 'b' --> 'c') --> ('b' --> 'a' --> 'c'))

-- \u x y -> x (u x y)
sp :: ((b -> c) -> a -> b) -> (b -> c) -> a -> c
sp = $(hole $ (('b' --> 'c') --> 'a' --> 'b') --> ('b' --> 'c') --> 'a' --> 'c')

-- \x y z w -> x ((y w) (z w))
f1 :: (b -> c) -> (d -> a -> b) -> (d -> a) -> (d -> c)
f1 = $(hole $ ('b' --> 'c') --> ('d' --> 'a' --> 'b') --> ('d' --> 'a') --> 'd' --> 'c')

t1 :: String
t1 = $(hole ''String)

t2 :: String -> Int
t2 = $(hole $ ''String --> ''Int)
