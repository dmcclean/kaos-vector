{-# LANGUAGE ClosedTypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Kaos.Systems
(
  Model,
  Dynamics,
  SamplingInfo(..),
  FunctionModel(..), NoiseModel(..),
  MeasurementModel, StateTransitionModel,
  discretize,
  linearizeAt
)
where

import Data.Matrix

data Model (d :: Dynamics) t x u y = Model 
                                           (SamplingInfo d t)
                                           (StateTransitionModel (Model d t x u y))
                                           (NoiseModel t x)
                                           (MeasurementModel (Model d t x u y))
                                           (NoiseModel t y)

data Dynamics = Continuous | Discrete

data SamplingInfo (d :: Dynamics) t where
  NoStep :: SamplingInfo Continuous t
  FixedStep :: t -> SamplingInfo Discrete t
  VariableStep :: SamplingInfo Discrete t

data NoiseModel t x where
  Noiseless :: NoiseModel x
  TimeInvariantNoise :: (MatrixDouble) -> NoiseModel x
  TimeVariantNoise :: (t -> Matrix Double) -> NoiseModel x

-- TODO: capture noise model and time (in-)dependence
data FunctionModel x u y where
  Linear :: (Matrix Double) -> (Matrix Double) -> FunctionModel x u y
  NonLinear :: (forall a.Num a => a->a) -> FunctionModel x u y

type family MeasurementModel where
  MeasurementModel (Model d t x u y) = FunctionModel x u y

type family StateTransitionModel where
  StateTransitionModel (Model Continuous t x u y) = FunctionModel x u x -- should be x' where x' has dimensions of x / t
  StateTransitionModel (Model Discrete t x u y) = FunctionModel x u x

discretize :: t -> Model Continuous t x u y -> Model Discrete t x u y
discretize = undefined

linearizeAt :: Model d t x u y -> x -> u -> Model d t x u y
linearizeAt = undefined
