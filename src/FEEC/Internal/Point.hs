{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module FEEC.Internal.Point(Point(Point),
             point,
             dimP,
             origin,
             unitP,
             cubic2Barycentric) where

import Data.List
import FEEC.Internal.Spaces
import FEEC.Utility.Print

-- | Points in R^n. A point describes a fixed position in space and
-- | can not be computed with.
data Point = Point [Double] deriving (Eq)

instance RenderVector Point where
    ncomps = dimP
    components (Point l) = l

-- TODO: abstract away via Functor+Applicative
{-
instance VectorSpace Point where
  type Fieldf Point = Fieldf [Double]
  addV (Point p) (Point q) = point $ addV p q
  sclV x (Point p) = point $ sclV x p
-}
-- TODO: only for now
-- could keep just Point or Vector and define one in terms of the other?
-- Use newtype
instance VectorSpace Point where
  type Fieldf Point = Double
  addV (Point p) (Point q) = Point $ zipWith (+) p q
  sclV x (Point p) = Point $ map (*x) p

instance Dimensioned Point where
    dim = dimP

instance Show Point where
    show p = "Point:\n" ++ show (printVector 2 p)

-- | Create point from list of components.
point :: [Double] -> Point
point = Point

dimP :: Point -> Int
dimP (Point l) = length l

-- | Return the origin in R^n.
origin :: Int -> Point
origin n = Point $ replicate n 0

-- | Point with the ith component 1.0 and all other components 0.0.
unitP :: Int -> Int -> Point
unitP n i = Point $ concat [replicate i 0.0, [1.0], replicate (n-i-1) 0.0]

-- | Convert point given in cubic coordinates to barycentric coordinates.
cubic2Barycentric :: Point -> Point
cubic2Barycentric (Point ts) = Point $ ls ++ [l]
    where (l,ls) = mapAccumL f 1 ts
          f acc t = (acc * (1 - t), t * acc)
