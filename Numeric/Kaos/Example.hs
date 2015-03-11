{-# LANGUAGE OverloadedStrings #-}

module Numeric.Kaos.Example

where

import Text.LaTeX
import Text.LaTeX.Packages.AMSMath
import Data.Matrix
import Numeric.Kaos.Vector

exampleDoc :: LaTeX
exampleDoc =
  documentclass [] article
 <> usepackage [] amsmath
 <> title "Example control system"
 <> author "J. Douglas McClean"
 <> document (maketitle <> "First, let's look at " <> someEqn <> " then " <> someMatrix <> "." <> eqn wikipediaNotation <> eqn2 wikipediaNotation)

someEqn :: LaTeX
someEqn = math $ (theta !: "x")

someMatrix :: LaTeX
someMatrix = math $ vmatrix Nothing m
  where
    m :: Matrix LaTeX
    m = fromList 5 1 [signum "abc", "123", derivative 1 theta "t", derivative 2 theta "t", "y + 2"]

eqn :: NotationalConvention -> LaTeX
eqn c = equation $ texy x <> "=" <> xMat
  where
    x = stateVector c
    xMat = matrixStyle c Nothing xMat'
    xMat' = fromList 5 1 ["x", derivativeStyle c True 1 "x" "t", theta, derivativeStyle c True 1 theta "t", "E"]

eqn2 :: NotationalConvention -> LaTeX
eqn2 c = equation $ samples (texy x) dist
  where
    x = stateVector c
    var = sigma ** 2
    dist = normalDistribution 0 var