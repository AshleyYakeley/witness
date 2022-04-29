module Data.Witness.Specific.Saturate where

import Data.Witness.General.Constraint
import Data.Witness.General.Representative
import Import

type SaturatedWitness :: forall k. (Type -> Type) -> k -> Type
data SaturatedWitness w t where
    NilSaturatedWitness :: forall w (t :: Type). w t -> SaturatedWitness w t
    ConsSaturatedWitness
        :: forall w kp kq (t :: kp -> kq). (forall (a :: kp). SaturatedWitness w (t a)) -> SaturatedWitness w t

instance forall (w :: Type -> Type). Representative w => Representative (SaturatedWitness w) where
    getRepWitness (NilSaturatedWitness w) =
        case getRepWitness w of
            Dict -> Dict
    getRepWitness (ConsSaturatedWitness wit) =
        case getRepWitness wit of
            Dict -> Dict

instance forall (w :: Type -> Type) kp kq (f :: kp -> kq). (Representative w) =>
             AllWitnessConstraint (Is (SaturatedWitness w)) f where
    allWitnessConstraint :: forall (t :: kp). Dict (Is (SaturatedWitness w) (f t))
    allWitnessConstraint =
        case representative @_ @(SaturatedWitness w) @f of
            ConsSaturatedWitness st -> getRepWitness $ st @t

instance forall (w :: Type -> Type) (t :: Type). Is w t => Is (SaturatedWitness w) t where
    representative = NilSaturatedWitness representative

instance forall (w :: Type -> Type) kp kq (t :: kp -> kq). ( Representative w
         , AllWitnessConstraint (Is (SaturatedWitness w)) t
         ) => Is (SaturatedWitness w) t where
    representative = let
        sw :: forall (a :: kp). SaturatedWitness w (t a)
        sw =
            case allWitnessConstraint @_ @_ @(Is (SaturatedWitness w)) @t @a of
                Dict -> representative
        in ConsSaturatedWitness sw

type SaturatedConstraintWitness c = SaturatedWitness (Compose Dict c)
