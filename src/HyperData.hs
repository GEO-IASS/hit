module HyperData where

import Data.Array.Repa as R

import HyperDataProperties

-- TODO: Make DIM2 after interleave is dealt with in reader
data HyperData = HyperCube { cubeData :: Array U DIM3 Double 
                           , cubeProperties :: HyperDataProperties } |
                 HyperLibrary { libData :: Array U DIM3 Double
                              , libProperties :: HyperDataProperties }

