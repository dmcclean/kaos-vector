{-# LANGUAGE OverloadedStrings #-}

module Numeric.Kaos.Example

where

import Text.LaTeX
import Text.LaTeX.Packages.AMSMath
import Numeric.Kaos.Notation

exampleDoc :: LaTeX
exampleDoc =
  documentclass [] article
 <> usepackage [] amsmath
 <> title "Example control system"
 <> author "J. Douglas McClean"
 <> document (maketitle <> "First, let's look at some equations." <> runEquation eqn2 <> runEquation eqn3)

{-
someMatrix :: LaTeX
someMatrix = math $ vmatrix Nothing m
  where
    m :: Matrix LaTeX
    m = fromList 5 1 [signum "abc", "123", derivative 1 theta "t", derivative 2 theta "t", "y + 2"]

eqn :: NotationalConvention -> LaTeX
eqn c = equation $ "x" <> "=" <> xMat
  where
    xMat = matrixStyle c Nothing xMat'
    xMat' = fromList 5 1 ["x", derivativeStyle c True 1 "x" "t", theta, derivativeStyle c True 1 theta "t", "E"]
-}

runEquation :: MathExpr -> LaTeX
runEquation = equation . formatMathExpr wikipediaNotation

eqn2 :: MathExpr
eqn2 = hasDistribution w dist
  where
    w = var $ metavariable' "w" (mathbf "w")
    s = var $ metavariable' "sigma" sigma
    variance = s ** 2
    dist = normalDistribution 0 variance

eqn3 :: MathExpr
eqn3 = expectation (w * wt) `equals` q
  where
    q = var $ metavariable "Q"
    w = var $ metavariable' "w" (mathbf "w")
    wt = transpose w