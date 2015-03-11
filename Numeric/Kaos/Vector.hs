{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Kaos.Vector

where

import Control.Applicative
import Data.Functor.Identity
import GHC.Generics
import Linear.Vector
import Text.LaTeX
import Text.LaTeX.Base.Class (fromLaTeX)
import Text.LaTeX.Packages.AMSMath

class Canonical f where
  quality :: (Floating a) => f a -> a
  quality = snd . normalize'
  normalize :: (Floating a) => f a -> f a
  normalize = fst . normalize'
  normalize' :: (Floating a) => f a -> (f a, a)
  normalize' x = (normalize x, quality x)
  {-# MINIMAL (quality, normalize) | normalize' #-}

class (Functor (MonomorphicVector vec)) => Vector (vec :: (* -> *) -> * -> *) where
  data MonomorphicVector vec :: * -> *
  toMono :: vec Identity a -> MonomorphicVector vec a
  fromMono :: MonomorphicVector vec a -> vec Identity a

-- Metavariable names
data Metavariable = Metavariable { basic :: String, pretty :: LaTeX, independent :: Bool }
                  | Estimate Metavariable
                  | Indexed Metavariable Metavariable
                  | Derivative Int Metavariable Metavariable
                  | Measurement Metavariable

instance Texy Metavariable where
  texy (Metavariable {pretty}) = fromLaTeX pretty
  texy (Estimate x) = hat $ texy x
  texy (Indexed sub x) = (texy x) !: (texy sub)
  texy (Derivative 0 x _) = texy x
  texy (Derivative 1 x t) | independent t = dot $ texy x
  texy (Derivative 2 x t) | independent t = ddot $ texy x
  texy (Derivative 3 x t) | independent t = dddot $ texy x
  texy (Derivative 1 x y) | otherwise = fromLaTeX $ ("d" <> texy x) / ("d" <> texy y)
  texy (Derivative n x y) = fromLaTeX $ ("d" ** n' <> x') / (("d" <> y') ** n')
    where
      n' = texy n
      x' = texy x
      y' = texy y
  texy (Measurement x) = (texy x) ^: "*"

-- Empty Vectors
data EmptyVector (f :: * -> *) (a :: *) = EmptyVector { } -- this is a record consructor for consistency with signal vectors that do have fields, to help TH if we end up needing to reify
  deriving (Eq, Functor, Generic)

instance Vector EmptyVector where
  data MonomorphicVector EmptyVector a = EmptyVector' {}
    deriving (Eq, Functor, Generic)
  toMono = const EmptyVector'
  fromMono = const EmptyVector

instance Applicative (EmptyVector f) where
  pure = const EmptyVector
  _ <*> _ = EmptyVector

instance Additive (EmptyVector f) where
  zero = EmptyVector

instance Applicative (MonomorphicVector EmptyVector) where
  pure = const EmptyVector'
  _ <*> _ = EmptyVector'

instance Additive (MonomorphicVector EmptyVector) where
  zero = EmptyVector' 
