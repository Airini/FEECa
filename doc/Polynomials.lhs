A polynomial $f(\vec{x})$ over $\R{n}$ may be written as a linear combination of monomials of the form

\begin{align}\label{eq:mon}
  f(\vec{x}) = \sum_i c_{\vec{\alpha}} \vec{x}^{\vec{\alpha}}
\end{align}

Here, $\vec{\alpha} = (\alpha_0,\ldots,\alpha_{n-1}) \in \mathrm{N}_0^{n}$ is a multi-index and the power of $\vec{x} = (x_0,\ldots,x_{n-1})$ with respect to a multi-index is defined as

\begin{align}
  \vec{x}^{\alpha} = \prod_{i=0}^{n-1}x_i^{\alpha_i}
\end{align}

The degree of a multi-index is just the some of its components.

In FEEC, we work with two representations of polynomials in $R{n}$. The first one, as given by \eqref{eq:mon}, will be referred to as the (standard) monomial basis.
%
The second one uses the Bernstein polynomials over a simplex as basis.
