{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Numeric.Kaos.Systems

where

import Data.Matrix

data Model (d :: Dynamics) t x y u = Model (SamplingInfo d t)

data Dynamics = Continuous | Discrete

data SamplingInfo (d :: Dynamics) t where
  NoStep :: SamplingInfo Continuous t
  FixedStep :: t -> SamplingInfo Discrete t
  VariableStep :: SamplingInfo Discrete t

data StateTransitionModel t x u x' where
  LinearContinuousStateTransition :: (Matrix Double) -> StateTransitionModel t x u x'
  NonLinearStateTransition :: (forall a.Num a => a->a) -> StateTransitionModel t x u x'