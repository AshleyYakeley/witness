module Data.Type.Witness.General.AllConstraint where

import Import

type AllConstraint :: forall kw kt. (kw -> Constraint) -> (kt -> kw) -> Constraint
class AllConstraint c w where
    allConstraint :: forall t. Dict (c (w t))

instance AllConstraint Show ((:~:) t) where
    allConstraint = Dict

allShow ::
       forall k (w :: k -> Type) (t :: k). AllConstraint Show w
    => w t
    -> String
allShow wt =
    case allConstraint @_ @_ @Show @w @t of
        Dict -> show wt
