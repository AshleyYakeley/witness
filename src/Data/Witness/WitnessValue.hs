module Data.Witness.WitnessValue where

import Data.Witness.Any
import Import

type WitnessValue :: forall k. (k -> Type) -> Constraint
class WitnessValue w where
    type WitnessValueType w :: Type
    witnessToValue :: forall t. w t -> WitnessValueType w
    valueToWitness :: forall r. WitnessValueType w -> (forall t. w t -> r) -> r

anyToValue ::
       forall k (w :: k -> Type). WitnessValue w
    => AnyW w
    -> WitnessValueType w
anyToValue (MkAnyW wt) = witnessToValue wt

valueToAny ::
       forall k (w :: k -> Type). WitnessValue w
    => WitnessValueType w
    -> AnyW w
valueToAny v = valueToWitness v MkAnyW
