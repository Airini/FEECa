

> class FiniteElement t r where
>   type Primitive t :: *
>   quadrature :: Int -> t -> (Primitive t -> r) -> r

> -- | Generalised integration expressed in terms of a quadrature method (with
> -- given number of nodes) associated with the 'FiniteElement' type pair.
> integrate :: (FiniteElement t r,
>              Function f (Primitive t), Scalar (Primitive t) ~ r)
%    => Int -> t -> f -> r
% integrate i t f = quadrature i t (f $$)


>   -- | 'Function' application:
>   -- > f $$ v ≈ f $ v

