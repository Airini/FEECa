{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Properties where

import FEEC.Internal.Form
import FEEC.Internal.Spaces
import FEEC.Internal.Vector
import FEEC.Utility.Utility (expSign)
import FEEC.Mask
import Test.QuickCheck as TQ


-- | Abstract properties
prop_linearity :: VectorSpace v => (Scalar v -> Scalar v -> Bool)
  -> (v -> Scalar v)
  -> Scalar v -> v -> v -> Bool
prop_linearity eq f c a b = mul c (f a) `eq` f (sclV c a)
                         && add (f a) (f b) `eq` f (addV a b)

prop_commutativity :: Eq t =>
    (f -> t) -> Binop f
  -> f -> f -> Bool
prop_commutativity render f x y = render (f x y) == render (f y x)

prop_operator_commutativity :: (b -> b -> Bool)
  -> Monop a -> Monop b -> (a -> b)
  -> a -> Bool
prop_operator_commutativity eq f1 f2 map a =
    map (f1 a) `eq` f2 (map a)

prop_operator2_commutativity :: (b -> b -> Bool)
  -> Binop a -> Binop b -> (a -> b)
  -> a -> a -> Bool
prop_operator2_commutativity eq f1 f2 map a1 a2 =
  map (f1 a1 a2) `eq` f2 (map a1) (map a2)

prop_associativity :: Eq t => (f -> t) -> Binop f
  -> f -> f -> f -> Bool
prop_associativity render f x y z = render (f (f x y) z) == render (f x (f y z))

prop_opRightIdentity :: Eq t => (f -> t) -> Binop f
  -> f -> f -> Bool
prop_opRightIdentity render f i x = render (f x i) == render x

prop_opLeftIdentity :: Eq t => (f -> t) -> Binop f
  -> f -> f -> Bool
prop_opLeftIdentity render f i x = render (f i x) == render x

prop_opId :: (Eq t, Arbitrary f, Show f) => (f -> t) -> Binop f
  -> f -> Property
prop_opId render f i = forAll arbitrary $ \x ->
                        prop_opLeftIdentity render f i x &&
                        prop_opRightIdentity render f i x

prop_distributivityA :: Eq t => (g -> t)
  -> Binop f -> Binop g -> (f -> g -> g)
  -> f -> f -> g -> Bool
prop_distributivityA render add1 add2 dot x y u =
  render (dot (add1 x y) u) == render (add2 (dot x u) (dot y u))

prop_distributivityB :: Eq t => (g -> t)
  -> Binop g -> (f -> g -> g)
  -> f -> g -> g -> Bool
prop_distributivityB render plus dot a u v =
  render (dot a (plus u v)) == render (plus (dot a u) (dot a v))


-- | Ring properties
-- Addition commutativity
propF_addComm :: (Eq f, Ring f) => f -> f -> Bool
propF_addComm = prop_commutativity id add
-- Addition associativity
propF_addAssoc :: (Eq f, Ring f) => f -> f -> f -> Bool
propF_addAssoc = prop_associativity id add -- (add x y) z == add x (add y z)
-- Addition identity element
propF_addId :: (Eq f, Ring f) => f -> Bool
propF_addId x = (add x addId == x) && (add addId x == x)

-- Product commutativity
propF_mulComm :: (Eq f, Ring f) => f -> f -> Bool
propF_mulComm = prop_commutativity id mul --mul x y == mul y x
-- Product  associativity
propF_mulAssoc :: (Eq f, Ring f) => f -> f -> f -> Bool
propF_mulAssoc = prop_associativity id mul --mul (mul x y) z == mul x (mul y z)
-- Addition identity element
propF_mulId :: (Eq f, Ring f) => f -> Bool
propF_mulId x = prop_opRightIdentity id mul mulId x && prop_opLeftIdentity id mul mulId x

-- inverses, distributivity


-- | Vector Space properties

-- Addition commutativity
propV_addComm :: (VectorSpace v, Eq v) => v -> v -> Bool
propV_addComm = prop_commutativity id addV--(addV x y) == (addV x y)
-- Addition associativity : to be instantiated with an appropriate type
propV_addAssoc :: (Eq v, VectorSpace v) => v -> v -> v -> Bool
propV_addAssoc = prop_associativity id addV

propV_sclTwice :: (VectorSpace v, Eq v) => Scalar v -> Scalar v -> v -> Bool
propV_sclTwice a b x = sclV a (sclV b x) == sclV (mul a b) x

propV_scladdFDistr :: (VectorSpace v, Eq v) => Scalar v -> Scalar v -> v -> Bool
propV_scladdFDistr = prop_distributivityA id add addV sclV

propV_scladdVDistr :: (VectorSpace v, Eq v) => Scalar v -> v -> v ->  Bool
propV_scladdVDistr = prop_distributivityB id addV sclV

-- | Alternating forms (graded algebra properties): other than the vector space properties

-- No need for evaluation on vectors for associativity
propA_wedgeAssoc :: (Algebra a, Eq a) => a -> a -> a -> Bool
propA_wedgeAssoc = prop_associativity id (/\)

-- instance Arbitrary Vector where
--   arbitrary = sized (TQ.vector >=> return . vector)

-- TODO: Eq?? would have to implement simplification + "canonising"
propA_wedgeAssocEvl :: (Ring f, VectorSpace f,
                        Projectable v (Scalar f), Scalar v ~ Scalar f)
                    => [v] -> Form f -> Form f -> Form f -> Bool
propA_wedgeAssocEvl vs = prop_associativity (#vs) (/\)

-- Will turn into check without evaluation if simplification + grouping of
-- terms with canonicalization is done
propA_wedgeAntiComm :: (Ring f, VectorSpace f,
                        Projectable v (Scalar f), Scalar v ~ Scalar f)
                    => Form f -> Form f -> [v] -> Bool
propA_wedgeAntiComm x y = \vs -> (x /\ y # vs) == (expSign jk (y /\ x # vs))
  where j = arity x
        k = arity y
        jk = j * k

propA_contractLeibniz :: (Ring f, VectorSpace f, Dimensioned v,
                          Projectable v f, Projectable v (Scalar v),
                          Scalar v ~ Scalar f)
                      => Form f -> Form f -> v -> [v] -> Bool
propA_contractLeibniz w t v vs =
  ((w /\ t) ⌟ v) # vs
    ==
  (addV ((w ⌟ v) /\ t) (sclV (expSign (arity w) addId) (w /\ (t ⌟ v)))) # vs
-- TODO: Abstract Leibniz rule away

(%#) :: (Ring f, VectorSpace f, Projectable v f, Scalar v ~ Scalar f, Dimensioned v)
     => Form f -> v -> Form f
(%#) = contract projection

propA_contractCochain :: (Ring f, VectorSpace f, Dimensioned v,
                          Projectable v f, Projectable v (Scalar v),
                          Scalar v ~ Scalar f)
                      => Form f -> v -> [v] -> Bool
propA_contractCochain w v = \vs ->
  (w ⌟ v ⌟ v) # vs == (zeroForm (arity w) (dimVec w)) # vs





-- XXX: ??? let there be dragons below?

--propA_basis basisV basisAF = undefined
-- Either. contraint basisAF to be associated to basisV OR derive from it
-- (yet to be implemented in Alternating)
-- choose sigma, rho by indexing randomly into the permutations k n (when proving for k)
-- wedge over pick (differences sigma) basisAF
-- pick (differences rho) basisV
-- property : rho == sigma ==> wedgeResult %$ picked vs == 1
--            rho /= sigma ==> wedgeResult %$ picked vs == 0


