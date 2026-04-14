{-# LANGUAGE GADTs, FlexibleContexts #-}

-- AST Definition
data KUTypeLang where
  TNum :: KUTypeLang
  TBool :: KUTypeLang
  deriving (Show, Eq)

data KULang where
  Num :: Int -> KULang
  Boolean :: Bool -> KULang
  Plus :: KULang -> KULang -> KULang
  Minus :: KULang -> KULang -> KULang
  Mult :: KULang -> KULang -> KULang
  Div :: KULang -> KULang -> KULang
  Exp :: KULang -> KULang -> KULang
  And :: KULang -> KULang -> KULang
  Or :: KULang -> KULang -> KULang
  Leq :: KULang -> KULang -> KULang
  IsZero :: KULang -> KULang
  If :: KULang -> KULang -> KULang -> KULang
  Between :: KULang -> KULang -> KULang -> KULang
  deriving (Show, Eq)

getNum :: KULang -> Maybe Int
getNum (Num n) = Just n
getNum _ = Nothing

getBool :: KULang -> Maybe Bool
getBool (Boolean b) = Just b
getBool _ = Nothing

-------------------------------
------ Project Exercises ------
-------------------------------

-- Part 1: Type Inference

-- Exercise 1
evalMonad :: KULang -> Maybe KULang
evalMonad (Num n) = Just (Num n)
evalMonad (Boolean b) = Just (Boolean b)

evalMonad (Plus x y) = do
  vx <- evalMonad x
  vy <- evalMonad y
  nx <- getNum vx
  ny <- getNum vy
  Just (Num (nx + ny))

evalMonad (Minus x y) = do
  vx <- evalMonad x
  vy <- evalMonad y
  nx <- getNum vx
  ny <- getNum vy
  Just (Num (nx - ny))

evalMonad (Mult x y) = do
  vx <- evalMonad x
  vy <- evalMonad y
  nx <- getNum vx
  ny <- getNum vy
  Just (Num (nx * ny))

evalMonad (Div x y) = do
  vx <- evalMonad x
  vy <- evalMonad y
  nx <- getNum vx
  ny <- getNum vy
  if ny == 0
    then Nothing
    else Just (Num (nx `div` ny))

evalMonad (Exp x y) = do
  vx <- evalMonad x
  vy <- evalMonad y
  nx <- getNum vx
  ny <- getNum vy
  if ny < 0
    then Nothing
    else Just (Num (nx ^ ny))

evalMonad (And x y) = do
  vx <- evalMonad x
  vy <- evalMonad y
  bx <- getBool vx
  by <- getBool vy
  Just (Boolean (bx && by))

evalMonad (Or x y) = do
  vx <- evalMonad x
  vy <- evalMonad y
  bx <- getBool vx
  by <- getBool vy
  Just (Boolean (bx || by))

evalMonad (Leq x y) = do
  vx <- evalMonad x
  vy <- evalMonad y
  nx <- getNum vx
  ny <- getNum vy
  Just (Boolean (nx <= ny))

evalMonad (IsZero x) = do
  vx <- evalMonad x
  nx <- getNum vx
  Just (Boolean (nx == 0))

evalMonad (If c t e) = do
  vc <- evalMonad c
  bc <- getBool vc
  if bc
    then evalMonad t
    else evalMonad e

evalMonad (Between x y z) = do
  vx <- evalMonad x
  vy <- evalMonad y
  vz <- evalMonad z
  nx <- getNum vx
  ny <- getNum vy
  nz <- getNum vz
  Just (Boolean (nx < ny && ny < nz))

-- Exercise 2
typeofMonad :: KULang -> Maybe KUTypeLang
typeofMonad (Num _) = Just TNum
typeofMonad (Boolean _) = Just TBool

typeofMonad (Plus x y) = do
  tx <- typeofMonad x
  ty <- typeofMonad y
  if tx == TNum && ty == TNum
    then Just TNum
    else Nothing

typeofMonad (Minus x y) = do
  tx <- typeofMonad x
  ty <- typeofMonad y
  if tx == TNum && ty == TNum
    then Just TNum
    else Nothing

typeofMonad (Mult x y) = do
  tx <- typeofMonad x
  ty <- typeofMonad y
  if tx == TNum && ty == TNum
    then Just TNum
    else Nothing

typeofMonad (Div x y) = do
  tx <- typeofMonad x
  ty <- typeofMonad y
  if tx == TNum && ty == TNum
    then Just TNum
    else Nothing

typeofMonad (Exp x y) = do
  tx <- typeofMonad x
  ty <- typeofMonad y
  if tx == TNum && ty == TNum
    then Just TNum
    else Nothing

typeofMonad (And x y) = do
  tx <- typeofMonad x
  ty <- typeofMonad y
  if tx == TBool && ty == TBool
    then Just TBool
    else Nothing

typeofMonad (Or x y) = do
  tx <- typeofMonad x
  ty <- typeofMonad y
  if tx == TBool && ty == TBool
    then Just TBool
    else Nothing

typeofMonad (Leq x y) = do
  tx <- typeofMonad x
  ty <- typeofMonad y
  if tx == TNum && ty == TNum
    then Just TBool
    else Nothing

typeofMonad (IsZero x) = do
  tx <- typeofMonad x
  if tx == TNum
    then Just TBool
    else Nothing

typeofMonad (If c t e) = do
  tc <- typeofMonad c
  tt <- typeofMonad t
  te <- typeofMonad e
  if tc == TBool && tt == te
    then Just tt
    else Nothing

typeofMonad (Between x y z) = do
  tx <- typeofMonad x
  ty <- typeofMonad y
  tz <- typeofMonad z
  if tx == TNum && ty == TNum && tz == TNum
    then Just TBool
    else Nothing

-- Exercise 3
interpTypeEval :: KULang -> Maybe KULang
interpTypeEval a = do
  _ <- typeofMonad a
  evalMonad a

-- Part 2: Optimizer

-- Exercise 1
optimize :: KULang -> KULang
optimize (Num n) = Num n
optimize (Boolean b) = Boolean b
optimize (Plus x (Num 0)) = optimize x
optimize (Plus x y) = Plus (optimize x) (optimize y)
optimize (Minus x y) = Minus (optimize x) (optimize y)
optimize (Mult x y) = Mult (optimize x) (optimize y)
optimize (Div x y) = Div (optimize x) (optimize y)
optimize (Exp x y) = Exp (optimize x) (optimize y)
optimize (And x y) = And (optimize x) (optimize y)
optimize (Or x y) = Or (optimize x) (optimize y)
optimize (Leq x y) = Leq (optimize x) (optimize y)
optimize (IsZero x) = IsZero (optimize x)
optimize (If (Boolean True) x y) = optimize x
optimize (If (Boolean False) x y) = optimize y
optimize (If c t e) = If (optimize c) (optimize t) (optimize e)
optimize (Between x y z) = Between (optimize x) (optimize y) (optimize z)

-- Exercise 2
interpOptEval :: KULang -> Maybe KULang
interpOptEval a = evalMonad (optimize a)