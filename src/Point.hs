{-# LANGUAGE TypeFamilies #-}

module Point where

import Spaces

-- | Points in R^n
data Point a = Point [a]

-- | Create point from list of components
point :: [a] -> Point a
point = Point

-- TODO: Dimension class?
dimP :: Point a -> Int
dimP (Point l) = length l

origin :: (Num a) => Int -> Point a
origin n = Point $ replicate n 0

-- | Position vector of given point
posVector :: (Rn v, Fieldf v ~ a) => Point a -> v
posVector (Point l) = fromList l
