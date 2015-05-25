# "Internal" (most important for implementor, less visible for user):
* Spaces.hs              -- General class definitions
* Point.hs
* Vector.hs
* Forms.hs
* Simplex.hs
* MultiIndex.hs

# Utilities
* Combinatorics.hs
* Print.hs               -- class RenderVector
* Discrete.hs            -- To be merged with Combinatorics
* GramSchmidt.hs
* Utility.hs
* Quadrature.hs

# Actual user should import:
* Polynomials.hs         -- Polynomial ~= [(Scalar, MultiIndex)] (lin. comb. of MultiIndex)
* Bernstein.hs           -- ~= (Polynomial, Simplex)
* DiffForms.hs           --
* FiniteElementSpaces.hs

* Mask.hs
* Demo.hs

# Less import / testing:
* CombinatoricsTest.hs
* FormsTest.hs
* PolynomialsTest.hs
* Properties.hs
* QuadratureTest.hs
* SimplexTest.hs
* Test.hs -- dummy file?
