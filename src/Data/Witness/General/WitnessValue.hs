module Data.Witness.General.WitnessValue where

import Data.Witness.Specific.Some
import Import

type WitnessValue :: forall k. (k -> Type) -> Constraint
class WitnessValue w where
    type WitnessValueType w :: Type
    witnessToValue :: forall t. w t -> WitnessValueType w
    valueToWitness :: forall r. WitnessValueType w -> (forall t. w t -> r) -> r

anyToValue ::
       forall k (w :: k -> Type). WitnessValue w
    => Some w
    -> WitnessValueType w
anyToValue (MkSome wt) = witnessToValue wt

valueToAny ::
       forall k (w :: k -> Type). WitnessValue w
    => WitnessValueType w
    -> Some w
valueToAny v = valueToWitness v MkSome
