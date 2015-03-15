{-# LANGUAGE OverloadedStrings #-}

module Numeric.Kaos.Example

where

import Data.Default
import Data.Matrix hiding (matrix, transpose)
import Text.LaTeX
import Text.LaTeX.Packages.AMSMath
import Numeric.Kaos.Notation

exampleDoc :: LaTeX
exampleDoc =
  documentclass [] article
 <> usepackage [] amsmath
 <> title "Example control system"
 <> author "J. Douglas McClean"
 <> document (maketitle <> "First, let's look at some equations." <> runEquation eqn1 <> runEquation eqn2 <> runEquation eqn3 <> runEquation eqn4)

runEquation :: MathExpr -> LaTeX
runEquation = equation . formatMathExpr def

eqn1 :: MathExpr
eqn1 = x `equals` xMat
  where
    x = variable "x"
    xMat = matrix xMat'
    xMat' = fromList 4 1 [variable "x", derivative True 1 (variable "x") (variable "t"), variable theta, variable "E"]

eqn2 :: MathExpr
eqn2 = hasDistribution w dist
  where
    w = variable (mathbf "w")
    s = variable sigma
    variance = s ** 2
    dist = normalDistribution 0 variance

eqn3 :: MathExpr
eqn3 = expectation (w * wt) `equals` q
  where
    q = variable "Q"
    w = variable (mathbf "w")
    wt = transpose w

eqn4 :: MathExpr
eqn4 = jacobianAt (variable "f") x (x `equals` estimate x)
  where
    x = variable "x"