module HyperData where

import Data.Array.Repa as R

import HyperDataProperties

-- TODO: Make DIM2 after interleave is dealt with in reader
data HyperData = HyperCube { cubeData :: Array U DIM1 Double 
                           , cubeProperties :: HyperDataProperties } |
                 HyperLibrary { libData :: Array U DIM1 Double
                              , libProperties :: HyperDataProperties }

