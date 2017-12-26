{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

-- | TIP solver for simply typed lambda calculus to automatically infer the code from type definitions using TemplateHaskell.
module Language.Haskell.Holes
  (
    hole
  , holeWith
  , defaultContext
  )
where


import Language.Haskell.TH (Type (ArrowT, ConT, AppT, VarT, ForallT), Q,
                            Exp (UnboundVarE, SigE, VarE, LamE, AppE), Name,
                            Pat(VarP),
                            pprint, runIO, mkName)
import Data.List (inits, tails, (++), length, map, take, head, tail,
                  null, concat, repeat, concatMap, zipWith)
import Data.Either (Either (Left, Right), rights, either)
import Control.Arrow (second)
import Control.Monad (liftM2, (>>=), (>>), return, fail)
import Prelude (Eq, Show,
                Maybe (Just, Nothing),
                Double, Float, Integer,
                show, ($), (.), putStrLn, (==), minBound, not, id, maybe, fromInteger)
import Data.Word (Word)
import Data.Int (Int)
import Data.Char (Char)
import Data.String (String)
import Data.Bool (Bool)
import Data.Functor ((<$>))


#if !MIN_VERSION_base (3,0,0)
-- | @since 3.0
instance Functor (Either a) where
  fmap :: Functor f => (a - b) -> f a -> f b
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)


-- | @since 3.0
instance Applicative (Either e) where
  pure          = Right
  Left  e <*> _ = Left e
  Right f <*> r = fmap f r
#endif


#if !MIN_VERSION_base (4,4,0)
-- | @since 4.4.0.0
instance Monad (Either e) where
  Left  l >>= _ = Left l
  Right r >>= k = k r
#endif


-- | Data type for type definitions.
data TypeDef =
  Atom Type
  | Imp TypeDef TypeDef
  deriving (Eq)


instance Show TypeDef where
  show (Atom name) = pprint name
  show (Imp a@(Imp _ _) b) = "(" ++ show a ++ ") -> " ++ show b
  show (Imp a b) = show a ++ " -> " ++ show b


-- | Data type for lambda-terms.
data Term =
  Var Name
  | Internal (Q Exp)
  | App Term Term
  | Lam Name Term


-- | Message type for code inference errors.
type ErrorMsg = (String, Maybe Context)


-- | List of assumptions
type Context = [(Term, TypeDef)]


-- | Used to allow user-defined contexts without exporting 'Term' and 'TypeDef'
-- constructors.
class ContextLike a where
  toContext :: a -> Context


instance ContextLike Context where
  toContext = id


-- | The user only needs 'Internal' and 'Atom'.
instance ContextLike [(Q Exp, Type)] where
  toContext = map (\(term, tp) -> (Internal term, Atom tp))


-- | Infinite list of unique variable names
vars :: [Name]
vars =
  map mkName $
  map return letters ++
  (zipWith (\c n -> c : show n) (concat $ repeat letters) $
    concatMap (\n -> take (length letters) $ repeat n) [1..])
  where letters = "abcdefghijklmnopqrstuvwxyz"


-- | Solve the type inhabitation problem for given type.
tip ::
  [Name] -- ^ List of new variables
  -> Context -- ^ Assumptions
  -> TypeDef -- ^ Goal
  -> Either ErrorMsg Term
tip vars context (Imp a b) =
  -- Create a new name for the variable bound by this lambda and
  -- update the context appropriately.
  let name = head vars in
    tip (tail vars) ((Var name, a) : context) b >>= Right . Lam name
tip vars context goal =
  -- Prove the goal by trying to reach it through the assumptions
  if null branches
  then Left ("Can't prove " ++ show goal, Just context)
  else Right $ head branches
  where
    branches :: [Term]
    branches = rights . map try $ pulls context

    try :: ((Term, TypeDef), Context) -> Either ErrorMsg Term
    try ((exp, Atom v), newCtx)
      | Atom v == goal = Right exp
    try ((exp, Imp a b), context') =
      tip vars context' a >>= \expA ->
      tip vars ((App exp expA, b) : context') goal
    try _ = Left ("", Nothing)

    -- pulls "abc" == [('a',"bc"),('b',"ac"),('c',"ab")]
    pulls :: [a] -> [(a, [a])]
    pulls xs = take (length xs) $ zipWith (second . (++)) (inits xs) breakdown
      where pull (x : xs) = (x, xs)
            breakdown = map pull (tails xs)


-- | Construct a term by given specification.
hole :: Q Exp -> Q Exp
hole = holeWith defaultContext


-- | Construct a term by given specification and additional context
holeWith :: ContextLike c => c -> Q Exp -> Q Exp
holeWith contextLike qexp = do
  let context = toContext contextLike
  exp <- qexp
  case extractTypeDef exp of
    Just (typeDef, name, tp) ->
      either (\(msg, mcontext) -> do
                 context <- printContext $ maybe [] id mcontext
                 fail $ msg ++ if not (null context) then
                                 "\nin the context:\n" ++ context
                               else "")
             (\r -> do
                 r <- toExp r
                 runIO . putStrLn $ "haskell-holes-th: '" ++ pprint name ++ "' := "
                   ++ pprint r ++ " :: " ++ show typeDef
                 return $ SigE r tp)
             (tip vars context typeDef)

    Nothing -> fail $ "Can't parse type definition in " ++ show exp
              ++ "\nHoles must be in form of '$(hole [| _ :: <place type "
              ++ "definition here> |])'"


-- | Extract type defintion - both as a 'TypeDef' and as a 'Type' with `forall`
-- quantifiers. Also return the name of a hole.
extractTypeDef :: Exp -> Maybe (TypeDef, Name, Type)
extractTypeDef (SigE (VarE n) tp@(ForallT _ _ t)) =
  (, n, tp) <$> getTypeDef t
extractTypeDef (SigE (VarE n) t) =
  (, n, t) <$> getTypeDef t
extractTypeDef (SigE (UnboundVarE n) tp@(ForallT _ _ t)) =
  (, n, tp) <$> getTypeDef t
extractTypeDef (SigE (UnboundVarE n) t) =
  (, n, t) <$> getTypeDef t
extractTypeDef _ = Nothing


-- | Disentangle the type definition.
getTypeDef :: Type -> Maybe TypeDef
getTypeDef a@(VarT _) = Just $ Atom a
getTypeDef a@(ConT _) = Just $ Atom a
getTypeDef (AppT (AppT ArrowT a) b) =
  liftM2 Imp (getTypeDef a) (getTypeDef b)
getTypeDef a@(AppT _ _) =
  Just $ Atom a
getTypeDef _ = Nothing


-- | Convert 'Term' to the code.
toExp :: Term -> Q Exp
toExp (Var n) = return $ VarE n
toExp (App a b) =
  liftM2 AppE (toExp a) (toExp b)
toExp l@(Lam _ _) =
  let (ls, t) = foldLambdas l in
    LamE ls <$> t
toExp (Internal qexp) = qexp


-- | Fold nested abstractions,
--
-- e.g. convert @ \a -> \b -> a b b @ to @ \a b -> a b b @
foldLambdas :: Term -> ([Pat], Q Exp)
foldLambdas (Lam a l@(Lam _ _)) =
  let (ls, t) = foldLambdas l in
    (VarP a : ls, t)
foldLambdas (Lam a b) =
  ([VarP a], toExp b)


printTerm :: Term -> Q String
printTerm (Var n) = return $ pprint n
printTerm (Internal qexp) = pprint <$> qexp
printTerm (App a b) =
  liftM2 (\a b -> "(" ++ a ++ " " ++ b ++ ")") (printTerm a) (printTerm b)
printTerm (Lam n t) =
  (("\\" ++ pprint n ++ " -> ") ++) <$> printTerm t


printContext :: Context -> Q String
printContext [] = return ""
printContext ((term, typeDef) : other) =
  liftM2 (\str strs ->
            str ++ " :: " ++ show typeDef ++ "\n" ++ strs)
         (printTerm term)
         (printContext other)


defaultContext :: [(Q Exp, Type)]
defaultContext =
  [
    ([| minBound |], ConT ''Bool),
    ([| minBound |], ConT ''Char),
    ([| minBound |], ConT ''Double),
    ([| minBound |], ConT ''Float),
    ([| minBound |], ConT ''Int),
    ([| minBound |], ConT ''Integer),
    ([| minBound |], ConT ''Word),
    ([| "" |], ConT ''String)
  ]
