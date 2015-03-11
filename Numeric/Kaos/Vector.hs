{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Kaos.Vector

where

import Control.Applicative
import Data.Functor.Identity
import GHC.Generics
import Linear.Vector
import Data.Matrix
import Text.LaTeX
import Text.LaTeX.Base.Class (LaTeXC, fromLaTeX)
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

metavariable :: String -> Metavariable
metavariable n = Metavariable { basic = n, pretty = fromString n, independent = False}

metavariable' :: String -> LaTeX -> Metavariable
metavariable' n l = Metavariable { basic = n, pretty = l, independent = False }

independentMetavariable :: String -> Metavariable
independentMetavariable n = Metavariable { basic = n, pretty = fromString n, independent = True}

instance Texy Metavariable where
  texy = fromLaTeX . formatMetavariable wikipediaNotation

formatMetavariable :: NotationalConvention -> Metavariable -> LaTeX
formatMetavariable _ (Metavariable {pretty}) = pretty
formatMetavariable c (Estimate x) = estimateStyle c x'
  where
    x' = formatMetavariable c x
formatMetavariable c (Indexed sub x) = indexedStyle c sub' x'
  where
    sub' = formatMetavariable c sub
    x' = formatMetavariable c x
formatMetavariable c (Derivative n x y) = derivativeStyle c ind n x' y'
  where
    ind = independent y
    x' = formatMetavariable c x
    y' = formatMetavariable c y
formatMetavariable c (Measurement x) = measurementStyle c x'
  where
    x' = formatMetavariable c x

dottedDerivative :: Int -> LaTeX -> LaTeX -> LaTeX
dottedDerivative 1 x _ = dot x
dottedDerivative 2 x _ = ddot x
dottedDerivative 3 x _ = dddot x
dottedDerivative n x y = derivative n x y

derivative :: Int -> LaTeX -> LaTeX -> LaTeX
derivative 0 x _ = x
derivative 1 x y = fromLaTeX $ ("d" <> x) / ("d" <> y)
derivative n x y = fromLaTeX $ ("d" ** n' <> x) / (("d" <> y) ** n')
  where
    n' = texy n

data NotationalConvention = NotationalConvention
                          {
                            estimateStyle :: LaTeX -> LaTeX,
                            indexedStyle :: LaTeX -> LaTeX -> LaTeX,
                            derivativeStyle :: Bool -> Int -> LaTeX -> LaTeX -> LaTeX,
                            measurementStyle :: LaTeX -> LaTeX,
                            matrixStyle :: forall a l.(Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l,
                            independentVariable :: Metavariable,
                            stateVector :: Metavariable,
                            outputVector :: Metavariable,
                            inputVector :: Metavariable,
                            stateTransitionMatrix :: Metavariable,
                            inputMatrix :: Metavariable,
                            outputMatrix :: Metavariable,
                            feedthroughMatrix :: Metavariable,
                            stateTransitionFunction :: Metavariable,
                            outputFunction :: Metavariable,
                            discreteStateIndex :: Metavariable
                          }

wikipediaNotation :: NotationalConvention
wikipediaNotation = NotationalConvention
                  {
                    estimateStyle = hat,
                    indexedStyle = flip (!:),
                    derivativeStyle = \case 
                                        True -> dottedDerivative
                                        False -> derivative,
                    measurementStyle = (^: "*"),
                    matrixStyle = bmatrix,
                    independentVariable = independentMetavariable "t",
                    stateVector = metavariable' "x" (mathbf "x"),
                    outputVector = metavariable' "y" (mathbf "y"),
                    inputVector = metavariable' "u" (mathbf "u"),
                    stateTransitionMatrix = metavariable "A",
                    inputMatrix = metavariable "B",
                    outputMatrix = metavariable "C",
                    feedthroughMatrix = metavariable "D",
                    stateTransitionFunction = metavariable "f",
                    outputFunction = metavariable "h",
                    discreteStateIndex = metavariable "k"
                  }

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
