# haskell-holes-th

[TIP](https://en.wikipedia.org/wiki/Type_inhabitation_problem) solver for [simply typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) + sum & product types, which can automatically infer code from type definitions (uses [TemplateHaskell](https://wiki.haskell.org/Template_Haskell)). It [may also be viewed](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence) as a prover for intuitionistic propositional logic.

## Usage

The following code sample shows the basic usage of the macro.

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.Holes

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

-- \x y z w -> x ((y w) (z w))
f1 :: (b -> c) -> (d -> a -> b) -> (d -> a) -> (d -> c)
f1 = $(hole [| f1 :: (b -> c) -> (d -> a -> b) -> (d -> a) -> d -> c |])

f2 :: (a, (b, c)) -> (a, (b, c))
f2 = $(hole [| f2 :: (a, (b, c)) -> (a, (b, c)) |])

f3 :: Either a (Either b c) -> Either (Either c b) a
f3 = $(hole [| f3 :: Either a (Either b c) -> Either (Either c b) a |])
```

Also check out [Test.hs](Test.hs).

## Limitations

- No ADT support

- No type synonyms support

- in STLC every typed term is strongly normalizing, so the type of [fixed-point combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator) can't be inhabited.

## See also

[djinn](https://github.com/augustss/djinn/) - a program synthesizer with algebraic data and type class support.
