module Data.Witness.WitnessValue where

import Data.Kind
import Data.Witness.Any

class WitnessValue (w :: k -> Type) where
    type WitnessValueType w :: Type
    witnessToValue :: forall (t :: k). w t -> WitnessValueType w
    valueToWitness :: forall r. WitnessValueType w -> (forall (t :: k). w t -> r) -> r

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
