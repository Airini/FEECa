{-# LANGUAGE AllowAmbiguousTypes#-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Properties where

--import Definitions
import Spaces
import Forms
import Vector
import Mask
import Test.QuickCheck
--import Data.Type.Natural


-- | Abstract properties
prop_commutativity :: Eq t => (f -> t) -> (f -> f -> f) -> f -> f -> Bool
prop_commutativity render f x y = render (f x y) == render (f y x)

prop_associativity :: Eq t => (f -> t) -> (f -> f -> f) -> f -> f -> f -> Bool
prop_associativity render f x y z = render (f (f x y) z) == render (f x (f y z))

prop_opRightIdentity :: Eq t => (f -> t) -> (f -> f -> f) -> f -> f -> Bool
prop_opRightIdentity render f i x = render (f x i) == render x

prop_opLeftIdentity :: Eq t => (f -> t) -> (f -> f -> f) -> f -> f -> Bool
prop_opLeftIdentity render f i x = render (f i x) == render x

prop_opId :: (Eq t, Arbitrary f, Show f) => (f -> t) -> (f -> f -> f) -> f -> Property
prop_opId render f i = forAll arbitrary $ \x ->
                        prop_opLeftIdentity render f i x &&
                        prop_opRightIdentity render f i x

-- | Field properties
-- Addition commutativity
propF_addComm :: (Eq f, Field f) => f -> f -> Bool
propF_addComm = prop_commutativity id add
-- Addition associativity
propF_addAssoc :: (Eq f, Field f) => f -> f -> f -> Bool
propF_addAssoc = prop_associativity id add -- (add x y) z == add x (add y z)
-- Addition identity element
propF_addId :: (Eq f, Field f) => f -> Bool
propF_addId x = (add x addId == x) && (add addId x == x)

-- Product commutativity
propF_mulComm :: (Eq f, Field f) => f -> f -> Bool
propF_mulComm = prop_commutativity id mul --mul x y == mul y x
-- Product  associativity
propF_mulAssoc :: (Eq f, Field f) => f -> f -> f -> Bool
propF_mulAssoc = prop_associativity id mul --mul (mul x y) z == mul x (mul y z)
-- Addition identity element
propF_mulId :: (Eq f, Field f) => f -> Bool
propF_mulId x = prop_opRightIdentity id mul mulId x && prop_opLeftIdentity id mul mulId x

-- inverses, distributivity


-- | Vector Space properties

-- Addition associativity : to be instantiated with an appropriate type
propV_addComm :: (VectorSpace v, Eq v) => v -> v -> Bool
propV_addComm = prop_commutativity id addV--(addV x y) == (addV x y)

-- Addition commutativity
propV_addAssoc :: (Eq v, VectorSpace v) => v -> v -> v -> Bool
propV_addAssoc = prop_associativity id addV

--- etc


-- | Alternating forms (... graded algebra properties): other than the vector space properties
-- TODO: add temporary wrapper for application of forms to 'Alternating'
--(%$) :: Form f -> Vector -> f
--(%$) = 

-- propA_wedgeAssoc ::
--propA_wedgeAssoc x y z = \vs -> ((x /\ y) /\ z) #vs == (x /\ (y /\ z)) #vs
propA_wedgeAssoc vs = prop_associativity (#vs) (/\) -- \vs -> ((x /\ y) /\ z) #vs == (x /\ (y /\ z)) #vs

-- NB: Cannot infer (no rules / type family / class instance) that k :+ j ~ j :+ k  ==> need for added constraint (second wedge product)
-- NB2: Fix ugly workaround for jk translation to an Int
-- (aside: some macro for this recurrent "undefined" pattern)
{-propA_wedgeAntiComm :: (Field f, (k :+ j) :<= n, (j :+ k) :<= n, Num f) =>
                       Form k n f -> Form j n f -> [Vex n f] -> Bool
propA_wedgeAntiComm x y = \vs -> ((x /\ y) %$ vs) == ((-1)^(jk) * ((y /\ x) %$ vs))
  where jv = (undefined :: j)
        kv = (undefined :: k)
        jk = (natToInt jv) * (natToInt kv)   -}
        -- jk = jv :* kv
        --jkb = (undefined :: j :* k)

{-
(%#) :: (Field f, k ~ (S predK)) => Form k n f -> Vex n f -> Form predK n f
(%#) = contract -- contraction
-}

-- Abstract Leibniz rule away
{-
propA_contractLeibniz :: (Field f, Num f, k ~ (S k'), l ~ (S l'), (k :+ l) :<= n) => 
                         -- S (k' :+ l') ~ (k' :+ l)) =>
                         Form k n f -> Form l n f -> Vex n f -> [Vex n f] -> Bool
propA_contractLeibniz w t v vs = ((w /\ t) %# v) %$ vs == (addV ((w %# v) /\ t) (sclV ((-1)^kv) (w /\ (t %# v)))) %$ vs
  where kv = natToInt (undefined :: k)
-}

--propA_contractAlt w v = w %# v %# v == const 0
-- For now :: assumes k >= 2 (until 'contraction' is fixed)
--propA_contractAlt' w v = \x -> (w %# v %# v) %$ x  == (const 0) %$ x


--propA_basis basisV basisAF = undefined
-- Either. contraint basisAF to be associated to basisV OR derive from it
-- (yet to be implemented in Alternating)
-- choose sigma, rho by indexing randomly into the permutations k n (when proving for k)
-- wedge over pick (differences sigma) basisAF
-- pick (differences rho) basisV
-- property : rho == sigma ==> wedgeResult %$ picked vs == 1
--            rho /= sigma ==> wedgeResult %$ picked vs == 0


