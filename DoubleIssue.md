https://github.com/Airini/FEECa/issues/3

Such as those converting to and from 'Double' in 'Field'. So far, they are seen in:

* (pretty) printing
* testing, e.g. 'eqNum' to account for margins of numerical error
* matrix related computations in those modules using HMatrix (note its
  replacement for Data.Matrix in branch WIP, but should still check
  for feasible component classes/types in the latter library)
* computing projections, due to 'sqrt', see 'project' in 'Simplex'
* computations involving 'factorial' and 'choose' in 'Combinatorics'

Initially, the existence of these class functions stemmed mostly from
the numerics behind integration of polynomials and other computations
of the like. Even though most practical uses of the library might
involve ℝn, it would be preferable to keep the classes neat and
general.

----------------

For two vectors `a` and `b` - to project `a` onto `b` compute the
scalar `s`

```
      dot a b
s =   -------
      dot b b
```

and scale `b` with that.

Then no `sqrt` is needed (for projection - it may still be needed
elsewhere).

TODO: fix name clash between Spaces.EuclideanSpace and Bernstein
