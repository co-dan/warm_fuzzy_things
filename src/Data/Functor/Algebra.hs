{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts   #-}
module Data.Functor.Algebra where

import Text.PrettyPrint as PP

-- * General types and definitions
type Algebra f a = f a -> a
type Coalgebra f a = a -> f a
newtype Mu f = Fix { unFix :: f (Mu f) }

-- | F-algebra catamorphism
cata :: (Functor f) => Algebra f a -> Mu f -> a
cata h = h . fmap (cata h) . unFix

-- | F-coalgebra anamorphism
ana :: (Functor f) => Coalgebra f a -> a -> Mu f
ana h = Fix . fmap (ana h) . h

-- | Hylomorphism
hylo :: (Functor f) => Algebra f b -> Coalgebra f a -> a -> b
hylo f g = cata f . ana g

-- | Equivalent definition of a hylomorphism
hylo2 :: (Functor f) => Algebra f b -> Coalgebra f a -> a -> b
hylo2 f g = f . fmap (hylo2 f g) . g

--  Dummy expression type/functor
data ExprF a = Const Int
             | Add   a a
             | Mult  a a
             deriving (Functor, Show)
type Expr = Mu ExprF

-- * Algebra examples

ppr :: Expr -> Doc
ppr = cata pprinter

-- An example of an ExprF-algebra that prerry-prints expression trees
pprinter :: ExprF Doc -> Doc
pprinter (Const  i) = text $ show i
pprinter (Add  a b) = PP.parens (a <+> text "+" <+> b)
pprinter (Mult a b) = PP.parens (a <+> text "*" <+> b)

--  Simple integer algebra
type ExprIntAlg = Algebra ExprF Int

-- siple interpreter
evalInt :: ExprIntAlg
evalInt = evalInt'
  where
    evalInt' (Const i)  = i
    evalInt' (Add  a b) = a + b
    evalInt' (Mult a b) = a * b

--  Initial algebras
type InitAlg f = Algebra f (Mu f)

evalInitAlg :: (Functor f) => InitAlg f
evalInitAlg = Fix

type ExprInitAlg = InitAlg ExprF

test :: Mu ExprF
test = Fix $ Add (Fix (Add one zero))
                 (Fix (Mult two one))
  where
    zero = Fix (Const 0)
    one = Fix (Const 1)
    two = Fix (Const 2)


--------------------------------------------------
optMult, optPlus :: Algebra ExprF Expr
optMult = optMult'
optPlus = optPlus'

optMult' :: ExprF Expr -> Expr
optMult' (Mult (Fix (Const 0)) _) = Fix (Const 0)
optMult' (Mult (Fix (Const 1)) e) = e
optMult' (Mult e (Fix (Const 1))) = e
optMult' (Mult _ (Fix (Const 0))) = Fix (Const 0)
optMult' e                        = Fix e

optPlus' :: ExprF Expr -> Expr
optPlus' (Add (Fix (Const 0)) e) = e
optPlus' (Add e (Fix (Const 0))) = e
optPlus' e                       = Fix e

-- * Coalgebra examples

plusTree :: Int -> ExprF Int
plusTree n
  | n < 0     = Const 1
  | otherwise = Add (n-1) (n-2)

ex1 :: Int -> Doc
ex1 = hylo pprinter plusTree

ex2 :: Int -> Int
ex2 = hylo evalInt plusTree

-- | Build up a balanced binary tree of addition
buildUp :: Int -> ExprF Int
buildUp n
  | n <= 0    = Const 1
  | otherwise = Add (n-1) (n-1)

-- pow2 n == 2^n
pow2 = hylo evalInt buildUp
