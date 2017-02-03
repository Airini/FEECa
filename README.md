# FEECa: Finite Element Exterior Calculus in Haskell [![Build Status](https://travis-ci.org/Airini/FEECa.svg?branch=master)](https://travis-ci.org/Airini/FEECa)

[FEECa (\['fi:ka\])](https://en.wikipedia.org/wiki/Fika_(Sweden)) is a library
implementing the mathematical framework that [the theory of finite element
exterior calculus (FEEC), developed by Arnold, Falk and Winther][arnold1],
provides for the discretization of partial differential equations (PDEs) that
allows for a universal treatment of a large number of physical problems.

[FEECa][docu] implements the abstract, mathematical concepts of FEEC and
provides a full-fledged basis form generated and framework for computations on
differential forms. It handles polynomial differential forms in arbitrary
dimensions and implements monomial as well as Bernstein bases for polynomials.
The package provides functionality to compute bases of the *P_r L^k* and
*P-_r L^k* spaces of finite element spaces based on the [geometric decomposition
proposed by Arnold, Falk and Winther][arnold2].

[arnold1]: http://dx.doi.org/10.1017/S0962492906210018
Douglas N. Arnold, Richard S. Falk, and Ragnar Winther: *Finite element exterior
calculus, homological techniques, and applications*. J. Acta Numerica. 2006

[arnold2]: http://dx.doi.org/10.1016/j.cma.2008.12.017
Douglas N. Arnold, Richard S. Falk, and Ragnar Winther: *Geometric
decompositions and local bases for spaces of finite element differential forms*.
J. Computer Methods in Applied Mechanics and Engineering. 2009.

[docu]: http://Airini.github.io/FEECa "Online documentation! (in the making)"
The [FEECa online documentation][docu] is under construction.

## The project

The project (Functional FEEC, initially) has been funded by the Information &
Communication Technology Area of Advance (ICT AoA) of Chalmers University and
is a collaboration between the Computational Mathematics (Department of
Mathematics) and Functional Programming (Department of Computer Science and
Engineering) groups at Chalmers University of Technology, Gothenburg, Sweden.

The resulting implemented package has been developed by (you can find contact
information in the package description file: `feeca.cabal`):
- Simon Pfreundschuh: MSc student in Physics
- Irene Lobo Valbuena: MSc/doctoral student in Computer Science

as joint work with:
- [Anders Logg](http://www.logg.org/anders): professor in Computational
  Mathematics
- [Patrik Jansson](https://www.chalmers.se/en/staff/Pages/patrik-jansson.aspx):
  professor in Computer Science. [github](https://github.com/patrikja)
- [Mary Sheeran](http://www.cse.chalmers.se/~ms): professor in Computer Science

## Presentation

2016-05-18: The project results are presented by
[Simon Pfreundscuh](http://easychair.org/smart-program/FEniCS'16/person11.html)
at the
[FENICS 2016 workshop](http://fenicsproject.org/featured/2016/fenics16_oslo.html).
[Programme](http://easychair.org/smart-program/FEniCS'16/2016-05-18.html#talk:23321).

## Documentation

The FEECa code is written using literate Haskell to combine source code and
documentation. The executable `doc` uses the
[Hakyll](https://jaspervdj.be/hakyll/) framework to compile the code into a
static web page and can be viewed in [your browser](localhost:8000) after
executing:

```
stack build feeca
stack exec doc rebuild
stack exec doc preview
```

or alternatively:

```
cabal run doc rebuild
cabal run doc server
```
