{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

-- | TIP solver for simply typed lambda calculus to automatically infer code from type definitions.
module Language.Haskell.Holes
  ( hole
  )
where

import           Data.Function
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
import           Control.Arrow
import           Control.Applicative
import           Data.Foldable
import           Language.Haskell.TH
import           Data.List
import           Data.Maybe


-- | Message type for code inference errors.
newtype ErrorMsg = ErrorMsg (String, Maybe Context)

instance Semigroup ErrorMsg where
  _                        <> b@(ErrorMsg (_, Just _)) = b
  a@(ErrorMsg (_, Just _)) <> _                        = a
  _                        <> b                        = b

instance Monoid ErrorMsg where
  mempty = mkError "Unknown" Nothing


-- | List of assumptions
type Context = [(Exp, Type)]


-- | Infinite list of unique variable names.
variables :: Variables
variables =
  map mkName $
  map pure letters ++
  (zipWith (\c n -> c : show (n :: Integer)) (concat $ repeat letters) $
    concatMap (\n -> take (length letters) $ repeat n) [1..])
  where letters = "abcdefghijklmnopqrstuvwxyz"


type Variables = [Name]


mkError :: String -> Maybe Context -> ErrorMsg
mkError str mctx = ErrorMsg (str, mctx)
{-# INLINE mkError #-}


type Env a = ExceptT ErrorMsg (State Variables) a


runEnv :: Env a -> Either ErrorMsg a
runEnv = flip evalState variables . runExceptT
{-# INLINE runEnv #-}


requestVar :: Env Name
requestVar = lift $ get >>= \(var : rest) -> put rest >> pure var


extractProduct
  :: Type
  -> Env ([Pat], Context)
extractProduct (TupleT _) = pure ([], [])
extractProduct (AppT i o) = do
  (pat, ctx) <- extractContext o
  first (++ [pat]) . second (ctx ++) <$> extractProduct i
extractProduct _ = throwE $ mkError "Not a product type" Nothing


extractContext
  :: Type
  -> Env (Pat, Context)
extractContext td =
  first TupP <$> extractProduct td <|> bindVariable
  where
    bindVariable = do
      var <- requestVar
      pure (VarP var, [(VarE var, td)])


prove
  :: Context
  -> Type
  -> Env Exp
-- If we are dealing with `acd -> csq`, extract pattern and context from
-- the antecedent, then prove the consequent within the new context.
prove ctx (AppT (AppT ArrowT acd) csq) =
  case acd of
    -- acd@(Either a b) -> csq
    (AppT (AppT (ConT c) a) b)
      | c == ''Either -> do
          -- Request a new variable for '\var -> case var of ...' expression
          lamVar <- requestVar
          -- Request two variables for both proof branches
          aVar <- requestVar
          aExpr <- prove ctx (AppT (AppT ArrowT a) csq)
          bVar <- requestVar
          bExpr <- prove ctx (AppT (AppT ArrowT b) csq)
          pure $
            LamE ([VarP lamVar]) $
            -- TODO: perform eta reduction where possible
            CaseE (VarE lamVar) [ Match (ConP 'Left  [VarP aVar]) (NormalB (AppE aExpr (VarE aVar))) []
                                , Match (ConP 'Right [VarP bVar]) (NormalB (AppE bExpr (VarE bVar))) []
                                ]
    -- acd -> csq
    _ -> do
      (pat, ctx') <- extractContext acd
      LamE [pat] <$> prove (ctx' ++ ctx) csq
prove ctx (AppT (AppT c a) b)
  | c == ConT ''Either =
    (prove ctx a <&> proceedWith 'Left) <|>
    (prove ctx b <&> proceedWith 'Right)
    where
      proceedWith con expr = AppE (ConE con) expr
prove ctx (AppT a b) = do
  aExpr <- prove ctx a
  bExpr <- prove ctx b
  pure (AppE aExpr bExpr)
prove ctx (TupleT n) =
  fromMaybe (throwE (mkError "Not a tuple" $ Just ctx)) $
  pure . ConE <$> nth n tupleConstructors
prove ctx goal = asum branches
  where
    branches :: [Env Exp]
    branches = map try $ pulls ctx

    try :: ((Exp, Type), Context) -> Env Exp
    try ((expr, AppT (AppT ArrowT a) b), context') =
      prove context' a >>= \exprA ->
      prove ((AppE expr exprA, b) : context') goal
    try ((expr, typ), _)
       | typ == goal = pure expr
    try _ =
      throwE (mkError "Can't infer type " $ Just ctx)

    -- pulls "abc" == [('a',"bc"),('b',"ac"),('c',"ab")]
    pulls xs = init $ zipWith pull (inits xs) (tails xs)

    pull xs (y:ys) = (y, xs ++ ys)
    pull _ _ = error "Impossible happened"

-- | Construct a term by given type specification.
hole :: Q Exp -> Q Exp
hole qexpr = do
  let context = []
  expr <- qexpr
  extractType expr &
    fmap (\(goal, holeName, inputType) ->
            either describeError (proceed goal holeName inputType) $
            runEnv $ prove context goal) &
    fromMaybe (onFail expr)
  where
    proceed :: Type -> Name -> Type -> Exp -> Q Exp
    proceed goal holeName inputType term = do
      let normalized = normalize term
      runIO . putStrLn $ "hole: '" ++ pprint holeName ++ "' := "
                       ++ pprint normalized ++ " :: " ++ pprint goal
      pure $ SigE normalized inputType

    describeError :: ErrorMsg -> Q Exp
    describeError (ErrorMsg (msg, mcontext)) = do
      context <- printContext $ fromMaybe [] mcontext
      fail $ msg ++ if not (null context)
                    then
                      "\nin the context:\n" ++ context
                    else
                      " (no context available)"

    onFail :: Exp -> Q Exp
    onFail expr = fail $ "Can't parse type definition in " ++ show expr
              ++ "\nHoles must be in form of '$(hole [| _ :: <place type "
              ++ "definition here> |])'"


-- | Extract type defintion - both as a 'Type' and as a 'Type' with `forall`
-- quantifiers. Also return the name of a hole.
extractType :: Exp -> Maybe (Type, Name, Type)
extractType (SigE (VarE n) tp@(ForallT _ _ t)) =
  pure (t, n, tp)
extractType (SigE (VarE n) t) =
  pure (t, n, t)
extractType (SigE (UnboundVarE n) tp@(ForallT _ _ t)) =
  pure (t, n, tp)
extractType (SigE (UnboundVarE n) t) =
  pure (t, n, t)
extractType _ = Nothing


-- | Fold nested lambda-abstractions,
--
-- e.g. convert @ \a -> \b -> a b b @ to @ \a b -> a b b @
foldLambdas :: Exp -> Maybe ([Pat], Exp)
foldLambdas (LamE pat l@(LamE _ _)) = do
  (ls, t) <- foldLambdas l
  pure (pat ++ ls, t)
foldLambdas (LamE pat b) =
  pure (pat, b)
foldLambdas _ = Nothing


-- | Pull out tuple constructors.
--
-- e.g. convert @(,,) a b c@ to @(a, b, c)@
normalizeProduct
  :: Exp
  -> Maybe Exp
normalizeProduct expr = case go 0 expr of
  Nothing -> Nothing
  Just exprs -> Just $ TupE $ reverse exprs
  where
    go :: Int -> Exp -> Maybe [Exp]
    go n (AppE (ConE tc) a) =
      if elemIndex tc tupleConstructors == Just (n + 1)
      then pure [a]
      else Nothing
    go n (AppE a b) = do
      exprs <- go (n+1) a
      pure (b : exprs)
    go _ _ = Nothing


-- | Apply 'normalizeProduct' and 'foldLambdas'.
normalize :: Exp -> Exp
normalize expr =
  case foldLambdas expr of
    Just (pats, expr') -> LamE pats $ normalize expr'
    Nothing -> case normalizeProduct expr of
      Nothing -> stepDown expr
      Just expr' -> normalize expr'
  where
    -- This function does NOT traverse the entire expression.
    -- TODO: add missing patterns.
    stepDown (AppE a b) = AppE (normalize a) (normalize b)
    stepDown (LamE pats b) = LamE pats (normalize b)
    stepDown (TupE exps) = TupE (map normalize exps)
    stepDown other = other


--  unlines $ map (\n -> "  , '("++ replicate (n - 1) ',' ++ ")")  [1..62]
tupleConstructors :: [Name]
tupleConstructors =
  [ '()
  , '(,)
  , '(,,)
  , '(,,,)
  , '(,,,,)
  , '(,,,,,)
  , '(,,,,,,)
  , '(,,,,,,,)
  , '(,,,,,,,,)
  , '(,,,,,,,,,)
  , '(,,,,,,,,,,)
  , '(,,,,,,,,,,,)
  , '(,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  , '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
  ]


printContext :: Context -> Q String
printContext [] = pure ""
printContext ((term, typeDef) : rest) =
  ((pprint term ++ " :: " ++ pprint typeDef ++ "\n") ++) <$> printContext rest


nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 0 (x : _)  = Just x
nth n (_ : xs) = nth (n - 1) xs


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

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

infixl 4 <&>
