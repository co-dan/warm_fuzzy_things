{-# LANGUAGE DeriveFunctor #-}
module Data.Functor.Algebra where

--  General types
newtype Algebra f a = Alg { unAlg :: (f a -> a) }
newtype Mu f = Fix (f (Mu f))


--  Dummy expression type/functor
data ExprF a = Const Int
             | Add   a a
             | Mult  a a
             deriving (Functor)

--  Simple integer algebra                      
type ExprIntAlg = Algebra ExprF Int

evalInt :: ExprIntAlg
evalInt = Alg evalInt'
  where
    evalInt' (Const i)  = i
    evalInt' (Add  a b) = a + b
    evalInt' (Mult a b) = a * b

--  Initial algebras
type InitAlg f = Algebra f (Mu f)

evalInitAlg :: (Functor f) => InitAlg f
evalInitAlg = Alg Fix

-- peel the 'Mu' off              
unFix :: Mu f -> f (Mu f)
unFix (Fix a) = a


type ExprInitAlg = InitAlg ExprF


-- | F-algebra catamorphism
cata :: (Functor f) => Algebra f a -> (Mu f -> a)
cata alg = unAlg alg . fmap (cata alg) . unFix


test :: Mu ExprF
test = Fix $ Add one
                 (Fix (Mult two two))  
  where
    one = Fix (Const 1)
    two = Fix (Const 2)


--------------------------------------------------
