# Very old description

Please refer to the newest documentation available online!

## "Internal" (most important for implementor, less visible for user):
* Spaces.hs              -- General class definitions
* Point.hs
* Vector.hs
* Form.hs                -- Form Scalar ~= [(Scalar, k-permutation of n)] (lin. comb. of k-permuations)
* Simplex.hs
* MultiIndex.hs

## Utilities
* Combinatorics.hs
* Print.hs               -- class RenderVector
* Discrete.hs            -- To be merged with Combinatorics
* GramSchmidt.hs
* Utility.hs
* Quadrature.hs

## Actual user should import:
* Polynomial.hs          -- Polynomial ~= [(Scalar, MultiIndex)] (lin. comb. of MultiIndex)
* Bernstein.hs           -- ~= (Polynomial, Simplex)
* DifferentialForm.hs    -- ~= Form Polynomial
* FiniteElementSpace.hs

* Mask.hs
* Demo.hs

## Less import / testing:
* CombinatoricsTest.hs
* FormTest.hs
* PolynomialTest.hs
* Properties.hs
* QuadratureTest.hs
* SimplexTest.hs
* Test.hs -- dummy file?

