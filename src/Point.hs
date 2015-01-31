{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Point where

import Spaces
import Vector

-- | Points in R^n
data Point = Point [Double] deriving (Eq, Show)

-- | Create point from list of components
point :: [Double] -> Point
point = Point

-- TODO: Dimension class?
dimP :: Point -> Int
dimP (Point l) = length l

origin :: Int -> Point
origin n = Point $ replicate n 0

-- | Position vector of given point
posVector :: Point -> Vector
posVector (Point l) = vector l
