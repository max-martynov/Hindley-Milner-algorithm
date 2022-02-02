{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Control.Monad.Except
import Data.Maybe
import Control.Monad
import Control.Monad.State


infixl 4 :@
infixr 3 :->

type Symb = String 

-- Терм
data Expr = Var Symb 
          | Expr :@ Expr
          | Lam Symb Expr
  deriving (Eq,Show)

-- Тип
data Type = TVar Symb 
          | Type :-> Type
  deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq,Show)

-- Возвращает список свободных переменных
freeVars :: Expr -> [Symb] 
freeVars (Var s) = [s]
freeVars (e :@ e') = (freeVars e) `union` (freeVars e')
freeVars (Lam s e) = (freeVars e) \\ [s]

-- Возвращает список свободных переменных типа
freeTVars :: Type -> [Symb]
freeTVars (TVar s) = [s]
freeTVars (t :-> t') = (freeTVars t) `union` (freeTVars t')

-- Расширяет контекст переменной с заданным типом
extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) s t = Env $ (s, t) : env

-- Возвращает список свободных типовых переменных контекста
freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env env) = foldr (\(s, t) set -> (freeTVars t) `union` set) [] env 

----

appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env xs) v = do
    let t = lookup v xs
    case t of
        Nothing -> throwError $ "There is no variable " ++ show v ++ " in the enviroment."
        Just tt -> return tt 

----

appSubsTy :: SubsTy -> Type -> Type
appSubsTy sub (t :-> t')            = (appSubsTy sub t) :-> (appSubsTy sub t')
appSubsTy (SubsTy sub) (TVar symb)  = 
    case lookup symb sub of
        Nothing -> TVar symb
        Just tt -> tt

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv sub (Env env) = Env $ do
    (s, t) <- env
    let newt = appSubsTy sub t
    return (s, newt)

---

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy (SubsTy sub1) (SubsTy sub2) = SubsTy $ (do
    (s2, t2) <- sub2
    let newt2 = appSubsTy (SubsTy sub1) t2
    return (s2, newt2)) ++ (do
        s1 <- (map fst sub1) \\ (map fst sub2)
        return (s1, fromJust $ lookup s1 sub1)
    )
    
instance Semigroup SubsTy where
    (<>) = composeSubsTy    

instance Monoid SubsTy where
    mempty = SubsTy []

---

unify :: MonadError String m => Type -> Type -> m SubsTy
unify t1 t2 | t1 == t2  = return mempty
            | otherwise = 
                case t1 of
                    TVar s1      -> if s1 `elem` (freeTVars t2)
                                    then throwError $ "Can't unify (" 
                                                 ++ show t1 
                                                 ++ ") with (" 
                                                 ++ show t2
                                                 ++ ")!"
                                    else return $ SubsTy $ [(s1, t2)]
                    
                    t1' :-> t1'' -> case t2 of
                                        TVar s2      -> unify t2 t1
                                        t2' :-> t2'' -> do
                                                            u2 <- unify t1'' t2''
                                                            u1 <- unify (appSubsTy u2 t1') (appSubsTy u2 t2')
                                                            return $ composeSubsTy u1 u2

---

equations :: MonadError String m => Env -> Expr -> Type -> m [(Type,Type)]
equations env e t = 
    evalStateT (equations' env e t) (freeTVars t `union` freeTVarsEnv env)

equations' :: MonadError String m => Env -> Expr -> Type -> StateT [Symb] m [(Type,Type)]
equations' env (Var x) t = do
                               t' <- appEnv env x
                               return [(t', t)]
equations' env (e :@ e') t = do
                                a <- getType "a"
                                eq1 <- equations' env e $ TVar a :-> t
                                eq2 <- equations' env e' $ TVar a
                                return $ eq1 `union` eq2
equations' env (Lam s e) t = do
                                a <- getType "a"
                                b <- getType "b"
                                eq1 <- equations' (extendEnv env s (TVar a)) e $ TVar b
                                let eq2 = [(TVar a :-> TVar b, t)]
                                return $ eq1 `union` eq2

getType :: MonadError String m => Symb -> StateT [Symb] m Symb
getType s = do
    types <- get
    let newType = fromJust(find (\typ -> not (typ `elem` types)) (genTypes s))
    put $ newType : types
    return newType

genTypes :: Symb -> [Symb] 
genTypes s = map (\x -> s ++ show x) [0..100]

term = Lam "y" $ Var "x"
env = Env [("x",TVar "a" :-> TVar "b")]

principlePair :: MonadError String m => Expr -> m (Env,Type)
principlePair e = do
    let fV = freeVars e
    let types = map TVar (genTypes "c")
    let env = Env $ zip fV types
    let superType = TVar "c228"
    eqs <- equations env e superType
    sub <- unify (uniteTypes $ map fst eqs) (uniteTypes $ map snd eqs)
    return $ (appSubsEnv sub env, appSubsTy sub superType)

uniteTypes :: [Type] -> Type
uniteTypes [t] = t
uniteTypes (t : types) = t :-> (uniteTypes types)

