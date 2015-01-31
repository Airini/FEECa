{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Point(Point(Point),
             point,
             dimP,
             origin,
             unitP,
             posVector) where

import Spaces
import Vector
import Print

-- | Points in R^n
data Point = Point [Double] deriving (Eq)

instance RenderVector Point where
    dim = dimP
    components (Point l) = l

instance Show Point where
    show p = "Point:\n" ++ (show $ printVector 2 p)

-- | Create point from list of components
point :: [Double] -> Point
point = Point

-- TODO: Dimension class?
dimP :: Point -> Int
dimP (Point l) = length l

origin :: Int -> Point
origin n = Point $ replicate n 0

-- | Point with the ith component 1 and all other components 0
unitP :: Int -> Int -> Point
unitP n i = Point $ concat [(replicate (i) 0.0), [1.0], (replicate (n-i-1) 0.0)]

-- | Position vector of given point
posVector :: Point -> Vector
posVector (Point l) = vector l
