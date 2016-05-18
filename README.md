# FEECa: Finite Element Exterior Calculus in Haskell

FEECa (\['fi:ka\]) is a library implementing the mathematical framework that
[the theory of finite element exterior calculus (FEEC), developed by Arnold,
Falk and Winther][arnold1], provides for the discretization of partial
differential equations (PDEs) that allows for a universal treatment of a large
number of physical problems.

[FEECa][docu] implements the abstract, mathematical concepts of FEEC and provides a
full-fledged basis form generated and framework for computations on differential
forms. It handles polynomial differential forms in arbitrary dimensions and
implements monomial as well as Bernstein bases for polynomials. The package
provides functionality to compute bases of the *P_r L^k* and *P-_r L^k* spaces
of finite element spaces based on the [geometric decomposition proposed by
Arnold, Falk and Winther][arnold2].


[arnold1]: http://dx.doi.org/10.1017/S0962492906210018
Douglas N. Arnold, Richard S. Falk, and Ragnar Winther: *Finite element exterior
calculus, homological techniques, and applications*. J. Acta Numerica. 2006

[arnold2]: http://dx.doi.org/10.1016/j.cma.2008.12.017
Douglas N. Arnold, Richard S. Falk, and Ragnar Winther: *Geometric
decompositions and local bases for spaces of finite element differential forms*.
J. Computer Methods in Applied Mechanics and Engineering. 2009.

[docu]: http://Airini.github.io/FEECa "Online documentation! (in the make)"

