{-# LANGUAGE DataKinds, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell, GADTs, FlexibleContexts #-}
--- My answer to this problem:
--- <http://www.haskell.org/pipermail/haskell-cafe/2014-January/112210.html>
module Image where
import Data.Singletons

type Point = (Float,Float)

$(singletons [d|
 data Shape' = Circle' | Rectangle' | Arbitrary'
            deriving (Eq)
 data Stroke' = Line' | Arc' | Spot'
            deriving (Eq)
 |])


data PenShape shape where
    Circle :: SingI Circle' => Float -> PenShape Circle'
    Rectangle :: SingI Rectangle' => Float -> Float -> PenShape Rectangle'
    ArbitraryPen :: PenShape Arbitrary'

class AllowedStroke (a::Stroke') (b::Shape') where

instance AllowedStroke Line' Circle'
instance AllowedStroke Line' Rectangle'
instance AllowedStroke Arc' Circle'
instance AllowedStroke Spot' Circle'
instance AllowedStroke Spot' Rectangle'
instance AllowedStroke Spot' Arbitrary'

data Stroke where
    Line :: AllowedStroke Line' a
         => Point -> Point -> PenShape a -> Stroke
    Arc  :: AllowedStroke Arc' a
         => Point -> Point -> Point -> PenShape a -> Stroke
    Spot :: AllowedStroke Spot' a
         => Point -> PenShape a -> Stroke

{-
h> :t Line (1,1) (1,1) (Circle 3)
Line (1,1) (1,1) (Circle 3) :: Stroke
h> :t Line (1,1) (1,1) (Rectangle 3 3)
Line (1,1) (1,1) (Rectangle 3 3) :: Stroke
h> :t Line (1,1) (1,1) ArbitraryPen

<interactive>:1:1:
    No instance for (AllowedStroke 'Line' 'Arbitrary')
      arising from a use of `Line'
    Possible fix:
      add an instance declaration for (AllowedStroke 'Line' 'Arbitrary')
    In the expression: Line (1, 1) (1, 1) ArbitraryPen
-}

--- unfortunately this still gives non-exhaustive pattern match
    --- warning :(
showStroke :: Stroke -> String
showStroke (Line _ _ (Circle _)) = "Line + Circle"
showStroke (Line _ _ (Rectangle _ _)) = "Line + Rect"
showStroke (Arc _ _ _ (Circle _)) = "Arc"
showStroke (Spot _  _) = "Spot"
