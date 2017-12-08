{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Haskell.Holes
  (
    (-->)
  , hole
  , holeWith
  , internals
  , Term (Internal)
  )
where


import Language.Haskell.TH
import Prelude hiding (lookup)
import Data.List (inits, tails)
import Data.Either (rights)
import Control.Arrow (second)


-- | Data type for type definitions.
data TypeDef =
  TVar Name
  | Imp TypeDef TypeDef
  deriving (Eq)


instance Show TypeDef where
  show (TVar name) = show name
  show (Imp a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"


-- | Data type for lambda-terms.
data Term =
  Var Name
  | Internal (Q Exp)
  | App Term Term
  | Lam Name Term


-- | Message type for code inference errors.
type ErrorMsg = String


-- | List of assumptions
type Context = [(Term, TypeDef)]


-- | Type class that generalizes everything that can be converted to a TypeDef
class TypeDefClass a where
  asTypeDef :: a -> TypeDef


instance TypeDefClass TypeDef where
  asTypeDef = id


instance TypeDefClass Char where
  asTypeDef = asTypeDef . (:[])


instance TypeDefClass String where
  asTypeDef = asTypeDef . mkName


instance TypeDefClass Name where
  asTypeDef = TVar


-- | Type constructor wrapper.
(-->) :: (TypeDefClass a, TypeDefClass b) => a -> b -> TypeDef
a --> b = Imp (asTypeDef a) (asTypeDef b)


infixr 6 -->


-- | Infinite list of unique variable names
vars :: [String]
vars =
  map (\n -> n) $
  map return letters ++
  (zipWith (\c n -> c : show n) (concat $ repeat letters) $
    concatMap (\n -> take (length letters) $ repeat n) [1..])
  where letters = "abcdefghijklmnopqrstuvwxyz"


-- | Solve the type inhabitation problem for given type.
tip ::
  [String] -- ^ List of new variables
  -> Context -- ^ Assumptions
  -> TypeDef -- ^ Goal
  -> Either ErrorMsg Term
tip vars ctx (Imp a b) =
  -- Create a new name for the variable bound by this lambda and
  -- update the context appropriately.
  let name = mkName (head vars) in
    case tip (tail vars) ((Var name, a) : ctx) b of
      Right exp -> Right $ Lam name exp
      x -> x
tip vars ctx goal =
  -- Prove the goal by trying to reach it through the assumptions
  if null branches
  then Left $ "Can't prove " ++ show goal
  else Right $ head branches
  where
    branches = rights . map try $ pulls ctx

    try :: ((Term, TypeDef), Context) -> Either ErrorMsg Term
    try ((exp, TVar v), newCtx)
      | TVar v == goal = Right exp
    try ((exp, Imp a b), newCtx) =
      case tip vars newCtx a of
        Right expA -> tip vars ((App exp expA, b) : newCtx) goal
        x -> x
    try _ = Left mempty


-- | Pull one element out for all elements. For example,
--
-- @
-- > pulls "abc" == [('a',"bc"),('b',"ac"),('c',"ab")]
-- @
pulls :: [a] -> [(a, [a])]
pulls xs = take (length xs) $ zipWith (second . (++)) (inits xs) breakdown
  where pull (x : xs) = (x, xs)
        breakdown = map pull (tails xs)


-- | Construct a term by given specification.
hole :: TypeDefClass a => a -> Q Exp
hole = holeWith internals


-- | Construct a term by given specification and additional context
holeWith :: TypeDefClass a => [(Term, TypeDef)] -> a -> Q Exp
holeWith internals exp =
  let typeDef = asTypeDef exp in
  case tip vars internals typeDef of
    Right r -> do
      r <- toExp r
      runIO . putStrLn $ "hole: " ++ pprint r ++ " :: " ++ show typeDef
      return r
    Left e -> fail e


-- | Convert 'Term' to the code.
toExp :: Term -> Q Exp
toExp (Var n) = return $ VarE n
toExp (App a b) = do
  a <- toExp a
  b <- toExp b
  return (AppE a b)
toExp (Lam a b) =
  toExp b >>= return . LamE [VarP a]
toExp (Internal qexp) = qexp


-- | Initial context.
internals =
  map (\(a, b) -> (Internal a, b))
  [
    ([| 0 :: Int |], TVar ''Int),
    ([| 0 :: Integer |], TVar ''Integer),
    ([| 0 :: Double |], TVar ''Double),
    ([| 0 :: Float |], TVar ''Float),
    ([| "" :: String |], TVar ''String),
    ([| ' ' :: Char |], TVar ''Char)
  ]
