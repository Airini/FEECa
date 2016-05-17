The purpose of finite element exterior calculus is the description of families of spaces of polynomial differential forms $\pl{r}{k}$ and $\pml{r}{k}$.
%
Here, $\pl{r}{k}$ is the space of polynomials differential $k$-forms of degree at most $r$.
%
The space $\pml{r}{k}$ is defined as

\begin{align}
\pml{r}{k} &= \{ \omega \in \pl{r}{k} || \kappa \omega \in \pl{r}{k-1} \}
\end{align}
where $\kappa$ is the Koszul operator.

FEECa provides the functions |prLkBasis| and |prmLKBasis| for the computations of bases for the spaces $\pl{r}{k}$ and $\pml{r}{k}$, respectively.
%
In addition to that, the |finiteElementSpace| function can be used to compute specific $\pl{r}{k}$ and $\pml{r}{k}$ spaces.

The computation of the bases is based on the formulas given by Arnold, Falk and Winther in \cite{ArnoldFalkWinther}.
