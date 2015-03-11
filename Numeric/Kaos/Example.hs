{-# LANGUAGE OverloadedStrings #-}

module Numeric.Kaos.Example

where

import Text.LaTeX
import Text.LaTeX.Packages.AMSMath
import Data.Matrix

exampleDoc :: LaTeX
exampleDoc =
	documentclass [] article
 <> usepackage [] amsmath
 <> title "Example control system"
 <> author "J. Douglas McClean"
 <> document (maketitle <> "First, let's look at " <> someEqn <> " then " <> someMatrix)

someEqn :: LaTeX
someEqn = math $ (theta !: "x")

someMatrix :: LaTeX
someMatrix = vmatrix Nothing m
  where
  	m :: Matrix LaTeX
  	m = fromList 4 1 ["abc", "123", theta, "y + 2"]