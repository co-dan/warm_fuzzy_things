{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Data.Functor.HAlgebra where

import Data.Functor.Identity

-------------------------------------------
-- Higher order F-algebras
-- algebras in the category of functors

type Nat f g = forall a. f a -> g a
-- ^ natural transformation between functors, monads, etc

newtype Algebra2 f g = Alg2 { unAlg2 :: Nat (f g) g }
-- TODO, rewrite using the composition of Functors

newtype Mu2 f a = Fix2 (f (Mu2 f) a)

unFix2 :: Mu2 f a -> f (Mu2 f) a
unFix2 (Fix2 a) = a

-- higher-order functor
class Functor2 h where
    hfmap :: Nat f g -> Nat (h f) (h g)

-- Our expression datatype
data ExprF f a where
    Const  :: Int                    -> ExprF f Int
    BConst :: Bool                   -> ExprF f Bool
    Add    :: f Int  -> f Int        -> ExprF f Int
    Mult   :: f Int  -> f Int        -> ExprF f Int
    If     :: f Bool -> f a   -> f a -> ExprF f a

-- Our algebra for the identity functor
algId :: Algebra2 ExprF Identity
algId = Alg2 evalId

evalId :: Nat (ExprF Identity) Identity
evalId (Const i)  = Identity $ i
evalId (BConst b) = Identity b
evalId (Add a b)  = Identity $ (runIdentity a) + (runIdentity b)
evalId (Mult a b) = Identity $ (runIdentity a) * (runIdentity b)
evalId (If a b c) = Identity $ case runIdentity a of
    True  -> runIdentity b
    False -> runIdentity c

-- Initial "higer-order" algebra
type Expr = Mu2 ExprF

--  Higer-order functor typeclass
instance Functor2 ExprF where
    hfmap _ (Const i)  = Const i
    hfmap _ (BConst b) = BConst b
    hfmap f (Add a b)  = Add (f a) (f b)
    hfmap f (Mult a b) = Mult (f a) (f b)
    hfmap f (If a b c) = If (f a) (f b) (f c)

-- "Higher-order" catamorphism
cata :: (Functor2 f, Functor g)
     => Algebra2 f g -> Nat (Mu2 f) g
cata alg2 = unAlg2 alg2 . hfmap (cata alg2) . unFix2


eval :: Expr a -> a
eval = runIdentity . cata algId

test :: Expr Int
test = Fix2 $ Add one
                 (Fix2 $ If false four two)
  where
    true  = Fix2 (BConst True)
    false = Fix2 (BConst False)
    one   = Fix2 (Const 1)
    two   = Fix2 (Const 2)
    four  = Fix2 (Mult two two)

