{-# LANGUAGE GADTs,FlexibleContexts #-} 

-- AST and Type Definitions
data KUTypeLang where
  TNum :: KUTypeLang
  TBool :: KUTypeLang
  deriving (Show,Eq)

data KULang where
  Num :: Int -> KULang
  Plus :: KULang -> KULang -> KULang
  Minus :: KULang -> KULang -> KULang
  Mult :: KULang -> KULang -> KULang
  Div :: KULang -> KULang -> KULang
  Exp :: KULang -> KULang -> KULang
  Boolean :: Bool -> KULang
  And :: KULang -> KULang -> KULang
  Or :: KULang -> KULang -> KULang
  Leq :: KULang -> KULang -> KULang
  IsZero :: KULang -> KULang
  If :: KULang -> KULang -> KULang -> KULang
  Between :: KULang -> KULang -> KULang -> KULang
  Bind :: String -> KULang -> KULang -> KULang
  Id :: String -> KULang
  deriving (Show,Eq)

type Env = [(String,KULang)]
type Cont = [(String,KUTypeLang)]

getNum :: KULang -> Maybe Int
getNum (Num n) = Just n
getNum _ = Nothing

getBool :: KULang -> Maybe Bool
getBool (Boolean b) = Just b
getBool _ = Nothing

lookupEnv :: String -> Env -> Maybe KULang
lookupEnv _ [] = Nothing
lookupEnv x ((y,v):ys)
  | x == y = Just v
  | otherwise = lookupEnv x ys

lookupCont :: String -> Cont -> Maybe KUTypeLang
lookupCont _ [] = Nothing
lookupCont x ((y,t):ys)
  | x == y = Just t
  | otherwise = lookupCont x ys

subst :: String -> KULang -> KULang -> KULang
subst x v (Num n) = Num n
subst x v (Boolean b) = Boolean b
subst x v (Plus a b) = Plus (subst x v a) (subst x v b)
subst x v (Minus a b) = Minus (subst x v a) (subst x v b)
subst x v (Mult a b) = Mult (subst x v a) (subst x v b)
subst x v (Div a b) = Div (subst x v a) (subst x v b)
subst x v (Exp a b) = Exp (subst x v a) (subst x v b)
subst x v (And a b) = And (subst x v a) (subst x v b)
subst x v (Or a b) = Or (subst x v a) (subst x v b)
subst x v (Leq a b) = Leq (subst x v a) (subst x v b)
subst x v (IsZero a) = IsZero (subst x v a)
subst x v (If c t e) = If (subst x v c) (subst x v t) (subst x v e)
subst x v (Between a b c) = Between (subst x v a) (subst x v b) (subst x v c)
subst x v (Bind y a b)
  | x == y = Bind y (subst x v a) b
  | otherwise = Bind y (subst x v a) (subst x v b)
subst x v (Id y)
  | x == y = v
  | otherwise = Id y

-------------------------------
------ Project Exercises ------
-------------------------------

-- Part 1: Adding Booleans

-- Exercise 1
evalDirect :: KULang -> (Maybe KULang)
evalDirect (Num n) = Just (Num n)
evalDirect (Boolean b) = Just (Boolean b)

evalDirect (Plus x y) = do
  vx <- evalDirect x
  vy <- evalDirect y
  nx <- getNum vx
  ny <- getNum vy
  Just (Num (nx + ny))

evalDirect (Minus x y) = do
  vx <- evalDirect x
  vy <- evalDirect y
  nx <- getNum vx
  ny <- getNum vy
  Just (Num (nx - ny))

evalDirect (Mult x y) = do
  vx <- evalDirect x
  vy <- evalDirect y
  nx <- getNum vx
  ny <- getNum vy
  Just (Num (nx * ny))

evalDirect (Div x y) = do
  vx <- evalDirect x
  vy <- evalDirect y
  nx <- getNum vx
  ny <- getNum vy
  if ny == 0
    then Nothing
    else Just (Num (nx `div` ny))

evalDirect (Exp x y) = do
  vx <- evalDirect x
  vy <- evalDirect y
  nx <- getNum vx
  ny <- getNum vy
  if ny < 0
    then Nothing
    else Just (Num (nx ^ ny))

evalDirect (And x y) = do
  vx <- evalDirect x
  vy <- evalDirect y
  bx <- getBool vx
  by <- getBool vy
  Just (Boolean (bx && by))

evalDirect (Or x y) = do
  vx <- evalDirect x
  vy <- evalDirect y
  bx <- getBool vx
  by <- getBool vy
  Just (Boolean (bx || by))

evalDirect (Leq x y) = do
  vx <- evalDirect x
  vy <- evalDirect y
  nx <- getNum vx
  ny <- getNum vy
  Just (Boolean (nx <= ny))

evalDirect (IsZero x) = do
  vx <- evalDirect x
  nx <- getNum vx
  Just (Boolean (nx == 0))

evalDirect (If c t e) = do
  vc <- evalDirect c
  bc <- getBool vc
  if bc
    then evalDirect t
    else evalDirect e

evalDirect (Between x y z) = do
  vx <- evalDirect x
  vy <- evalDirect y
  vz <- evalDirect z
  nx <- getNum vx
  ny <- getNum vy
  nz <- getNum vz
  Just (Boolean (nx < ny && ny < nz))

evalDirect (Bind x v b) = do
  vv <- evalDirect v
  evalDirect (subst x vv b)

evalDirect (Id _) = Nothing

-- Exercise 2
evalDeferred :: Env -> KULang -> (Maybe KULang)
evalDeferred _ (Num n) = Just (Num n)
evalDeferred _ (Boolean b) = Just (Boolean b)

evalDeferred env (Plus x y) = do
  vx <- evalDeferred env x
  vy <- evalDeferred env y
  nx <- getNum vx
  ny <- getNum vy
  Just (Num (nx + ny))

evalDeferred env (Minus x y) = do
  vx <- evalDeferred env x
  vy <- evalDeferred env y
  nx <- getNum vx
  ny <- getNum vy
  Just (Num (nx - ny))

evalDeferred env (Mult x y) = do
  vx <- evalDeferred env x
  vy <- evalDeferred env y
  nx <- getNum vx
  ny <- getNum vy
  Just (Num (nx * ny))

evalDeferred env (Div x y) = do
  vx <- evalDeferred env x
  vy <- evalDeferred env y
  nx <- getNum vx
  ny <- getNum vy
  if ny == 0
    then Nothing
    else Just (Num (nx `div` ny))

evalDeferred env (Exp x y) = do
  vx <- evalDeferred env x
  vy <- evalDeferred env y
  nx <- getNum vx
  ny <- getNum vy
  if ny < 0
    then Nothing
    else Just (Num (nx ^ ny))

evalDeferred env (And x y) = do
  vx <- evalDeferred env x
  vy <- evalDeferred env y
  bx <- getBool vx
  by <- getBool vy
  Just (Boolean (bx && by))

evalDeferred env (Or x y) = do
  vx <- evalDeferred env x
  vy <- evalDeferred env y
  bx <- getBool vx
  by <- getBool vy
  Just (Boolean (bx || by))

evalDeferred env (Leq x y) = do
  vx <- evalDeferred env x
  vy <- evalDeferred env y
  nx <- getNum vx
  ny <- getNum vy
  Just (Boolean (nx <= ny))

evalDeferred env (IsZero x) = do
  vx <- evalDeferred env x
  nx <- getNum vx
  Just (Boolean (nx == 0))

evalDeferred env (If c t e) = do
  vc <- evalDeferred env c
  bc <- getBool vc
  if bc
    then evalDeferred env t
    else evalDeferred env e

evalDeferred env (Between x y z) = do
  vx <- evalDeferred env x
  vy <- evalDeferred env y
  vz <- evalDeferred env z
  nx <- getNum vx
  ny <- getNum vy
  nz <- getNum vz
  Just (Boolean (nx < ny && ny < nz))

evalDeferred env (Bind x v b) = do
  vv <- evalDeferred env v
  evalDeferred ((x,vv):env) b

evalDeferred env (Id x) = lookupEnv x env

-- Exercise 3
testEvals :: KULang -> Bool
testEvals a = evalDirect a == evalDeferred [] a

-- Part 2: Type Checking

--Exercise 1
typeofMonad :: Cont -> KULang -> (Maybe KUTypeLang)
typeofMonad _ (Num _) = Just TNum
typeofMonad _ (Boolean _) = Just TBool

typeofMonad cont (Plus x y) = do
  tx <- typeofMonad cont x
  ty <- typeofMonad cont y
  if tx == TNum && ty == TNum
    then Just TNum
    else Nothing

typeofMonad cont (Minus x y) = do
  tx <- typeofMonad cont x
  ty <- typeofMonad cont y
  if tx == TNum && ty == TNum
    then Just TNum
    else Nothing

typeofMonad cont (Mult x y) = do
  tx <- typeofMonad cont x
  ty <- typeofMonad cont y
  if tx == TNum && ty == TNum
    then Just TNum
    else Nothing

typeofMonad cont (Div x y) = do
  tx <- typeofMonad cont x
  ty <- typeofMonad cont y
  if tx == TNum && ty == TNum
    then Just TNum
    else Nothing

typeofMonad cont (Exp x y) = do
  tx <- typeofMonad cont x
  ty <- typeofMonad cont y
  if tx == TNum && ty == TNum
    then Just TNum
    else Nothing

typeofMonad cont (And x y) = do
  tx <- typeofMonad cont x
  ty <- typeofMonad cont y
  if tx == TBool && ty == TBool
    then Just TBool
    else Nothing

typeofMonad cont (Or x y) = do
  tx <- typeofMonad cont x
  ty <- typeofMonad cont y
  if tx == TBool && ty == TBool
    then Just TBool
    else Nothing

typeofMonad cont (Leq x y) = do
  tx <- typeofMonad cont x
  ty <- typeofMonad cont y
  if tx == TNum && ty == TNum
    then Just TBool
    else Nothing

typeofMonad cont (IsZero x) = do
  tx <- typeofMonad cont x
  if tx == TNum
    then Just TBool
    else Nothing

typeofMonad cont (If c t e) = do
  tc <- typeofMonad cont c
  tt <- typeofMonad cont t
  te <- typeofMonad cont e
  if tc == TBool && tt == te
    then Just tt
    else Nothing

typeofMonad cont (Between x y z) = do
  tx <- typeofMonad cont x
  ty <- typeofMonad cont y
  tz <- typeofMonad cont z
  if tx == TNum && ty == TNum && tz == TNum
    then Just TBool
    else Nothing

typeofMonad cont (Bind x v b) = do
  tv <- typeofMonad cont v
  typeofMonad ((x,tv):cont) b

typeofMonad cont (Id x) = lookupCont x cont

--Exercise 2
interpret :: KULang -> (Maybe KULang)
interpret a = do
  _ <- typeofMonad [] a
  evalDeferred [] a