module HyperData where

import Data.Array.Repa as R

import HyperDataProperties

data HyperData = HyperCube { cubeData :: Array U DIM2 Double 
                           , cubeProperties :: HyperDataProperties } |
                 HyperLibrary { libData :: Array U DIM2 Double
                              , libProperties :: HyperDataProperties }

