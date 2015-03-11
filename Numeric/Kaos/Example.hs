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
 <> document (maketitle <> "First, let's look at " <> someEqn <> " then " <> someMatrix)

someEqn :: LaTeX
someEqn = math $ (theta !: "x")

someMatrix :: LaTeX
someMatrix = math $ vmatrix Nothing m
  where
  	m :: Matrix LaTeX
  	m = fromList 5 1 [signum "abc", "123", derivative 1 theta "t", derivative 2 theta "t", "y + 2"]