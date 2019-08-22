module MyPrelude where

import qualified RIO.List.Partial as List'
import RIO.Seq (Seq)
import qualified RIO.Seq as Seq

{- This operator already exists as the symbol '&' but I think that is horrible
   notation and the |> ('pipe') operator is found in this form in other
   languages such as F# and Elixir. It can be naturally read as 'pass the result
   of the thing on the left to the function on the right'. Such an operator is
   even more commonly seen in shell scripts and on the command line as the
   symbol '|'.
-}
infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x


{- Define rotate as a function which rotates a sequence or list to the right by
   one element.

   For example, rotate [0, 1, 2, 3]                     = [3, 0, 1, 2]
                rotate [0, 1, 2, 3] |> rotate           = [2, 3, 0, 1]
                rotate [0, 1, 2, 3] |> rotate |> rotate = [1, 2, 3, 0].
-}
class Rotatable a where
  rotate :: a -> a

instance Rotatable (Seq a) where
  rotate s@(Seq.Empty)           = s
  rotate s@(Seq.Empty Seq.:|> _) = s
  rotate   (xs Seq.:|> end)      = end Seq.<| xs

instance Rotatable [a] where
  rotate []  = []
  rotate [x] = [x]
  rotate xs  = (List'.last xs) : (List'.init xs)
