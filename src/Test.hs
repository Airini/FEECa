{-# LANGUAGE GADTs #-}
module Test where

import Forms
import DiffForms
import Vector

--type Form f = (Int -> Int -> [(f, [Int])])

data Fff f = Ff {ar :: Int, cs :: [(f, [Int])]}
type Fdu f v = Fff f -> [v] -> f

data Alt where
  Alter :: Form Double -> (Int -> Vector -> Double) -> Alt -- [Vector] -> Double
  Add   :: Alt -> Alt -> Alt
  Wedge :: Alt -> Alt -> Alt
{-
alter (Alter w dxx) = refine dxx w
alter (Add w t)     = alter 
-}
alter = refine dxV
alternating :: Form Double -> [Vector] -> Double
alternating = refine dxV


{-
  Vector space forms act on: dimension n
    want: not to have to mention the dimension in calls to operators such as
          the exterior derivative
    problem: has to nonetheless be specified somewhere eg: when constructing
             the form
    otherwise: we can only know such a parameter n when applying to vectors
               entailing:
        -> all computation delayed until then (ext.deriv...)
        -> no way of printing the result of an exterior derivative as a form,
           only when there is some witness of the vector space it is to be
           applied on

  Forms as functions: there has to be a run function at some moment in order
  to have symbolic representation.
    could have symbolic forms (as they are now) and a command to declare the
    "runnable" ones from those symbolic ones, eg:
      dx1 = dx 1
      form = refine dxV
      dx_1 = form dx1
    problem: those processed by form can no longer be used symbolically like
             before (eg: cannot compute the exterior derivative)
    IF dependent types are used (for arity of forms) could probably still
    define addition and wedge product for those, but because of typing,
    correctness guarantees are reduced (if something defined freely as
    [Vector] -> Double is employed)
  
  Symbols for operations (class Num):
    for Field, would be fine
    for vector spaces: import Num qualified, create new class with such ops
      (see very rough tt.hs); might run into trouble when instantiating
      appropriately the different classes (scaling vectors...). Could
      potentially become a pain to write other expressions

-}



