# haskell-holes-th

[TIP](https://en.wikipedia.org/wiki/Type_inhabitation_problem) solver for [simply typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) to automatically infer the code from type definitions using [TemplateHaskell](https://wiki.haskell.org/Template_Haskell). It may also be viewed as a prover for intuitionistic propositional logic with only implication allowed.

## Usage

The following example shows the basic usage of the macro.

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
```

Also check out [Test.hs](Test.hs).

## Limitations

- Only atomic types from the given context can be inferred. Use `holeWith` instead of `hole` to specify a non-default context. The default one contains `Bool`, `Char`, `Double`, `Float`, `Int`, `Integer`, `Word` and `String`.

- Haskell's type system is more rich than simply typed lambda calculus (it allows polymorphism), so some of the types that have corresponding definitions in Haskell can't be inferred. Also, in STLC every typed term is strongly normalizing, so the type of [fixed-point combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator) can't be inhabited.

## Custom context

Any atomic type can be added to the context by constructing a quoted expression of that type and a type itself (as an [Exp from TemplateHaskell](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Exp)).

```
t3 :: Maybe Int
t3 = $(holeWith
       -- context = [Just 0 :: Maybe Int]
       [([| Just 0 |], AppT (ConT ''Maybe) (ConT ''Int))]
       [| mi :: Maybe Int |])

```

If the type do not correspond to the quoted value, *the code containing the inferred term* will not compile, but no warnings or errors will be shown if the quoted value is never used.

Type definition in terms of `Exp` can be retrieved from `ghci` as follows:

```
$ ghci -XTemplateHaskell
Prelude> :m + Language.Haskell.TH
Prelude Language.Haskell.TH> runQ [| _ :: Either (Maybe Int) String |] >>= print
SigE (UnboundVarE _) (AppT (AppT (ConT Data.Either.Either) (AppT (ConT GHC.Base.Maybe) (ConT GHC.Types.Int))) (ConT GHC.Base.String))
```
The part starting after `(UnboundVarE _)` is needed.
