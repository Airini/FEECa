# FEECa: Finite Element Exterior Calculus in Haskell

FEECa (\['fi:ka\]) is a library implementing the mathematical framework that
[the theory of finite element exterior calculus (FEEC), developed by Arnold,
Falk and Winther][arnold1], provides for the discretization of partial
differential equations (PDEs) that allows for a universal treatment of a large
number of physical problems.

FEECa implements the abstract, mathematical concepts of FEEC and provides a
full-fledged basis form generated and framework for computations on differential
forms. It handles polynomial differential forms in arbitrary dimensions and
implements monomial as well as Bernstein bases for polynomials. The package
provides functionality to compute bases of the

P\_r &#039B;^k

and

P^(-)\_r &#039B;^k

*P_r\Lambda^k$ and $\mathcal{P}^-_r\Lambda^k$ spaces of
finite element spaces based on the
[geometric decomposition proposed by Arnold, Falk and Winther][arnold2].


[arnold1]: http://dx.doi.org/10.1017/S0962492906210018
Douglas N. Arnold, Richard S. Falk, and Ragnar Winther: *Finite element exterior
calculus, homological techniques, and applications*. J. Acta Numerica. 2006

[arnold2]: http://dx.doi.org/10.1016/j.cma.2008.12.017
Douglas N. Arnold, Richard S. Falk, and Ragnar Winther: *Geometric
decompositions and local bases for spaces of finite element differential forms*.
J. Computer Methods in Applied Mechanics and Engineering. 2009.


# "Internal" (most important for implementor, less visible for user):
* Spaces.hs              -- General class definitions
* Point.hs
* Vector.hs
* Form.hs                -- Form Scalar ~= [(Scalar, k-permutation of n)] (lin. comb. of k-permuations)
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
* Polynomial.hs          -- Polynomial ~= [(Scalar, MultiIndex)] (lin. comb. of MultiIndex)
* Bernstein.hs           -- ~= (Polynomial, Simplex)
* DifferentialForm.hs    -- ~= Form Polynomial
* FiniteElementSpace.hs

* Mask.hs
* Demo.hs

# Less import / testing:
* CombinatoricsTest.hs
* FormTest.hs
* PolynomialTest.hs
* Properties.hs
* QuadratureTest.hs
* SimplexTest.hs
* Test.hs -- dummy file?
