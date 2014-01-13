{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts   #-}
module Data.Functor.Algebra where

import Text.PrettyPrint as PP

--  General types
type Algebra f a = f a -> a
newtype Mu f = Fix (f (Mu f))


--  Dummy expression type/functor
data ExprF a = Const Int
             | Add   a a
             | Mult  a a
             deriving (Functor, Show)

type Expr = Mu ExprF

ppr :: Expr -> Doc
ppr = cata pprinter

pprinter :: ExprF Doc -> Doc
pprinter (Const  i) = text $ show i
pprinter (Add  a b) = PP.parens (a <+> text "+" <+> b)
pprinter (Mult a b) = PP.parens (a <+> text "*" <+> b)

--  Simple integer algebra
type ExprIntAlg = Algebra ExprF Int

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

-- peel the 'Mu' off
unFix :: Mu f -> f (Mu f)
unFix (Fix a) = a


type ExprInitAlg = InitAlg ExprF


-- | F-algebra catamorphism
cata :: (Functor f) => Algebra f a -> Mu f -> a
cata alg = alg . fmap (cata alg) . unFix


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


-- algComp :: Algebra f a -> Algebra g (Mu f) -> Algebra f a

-- opt+ :: ExprF (Mu ExprF) -> Mu ExprF
-- opt* :: ExprF (Mu ExprF) -> Mu ExprF

-- x :: f a -> a
-- y :: g (Mu f) -> Mu f

-- cata x . cata y :: Mu g -> a

-- f ~ ExprF, a ~ Expr ~ Mu ExprF
