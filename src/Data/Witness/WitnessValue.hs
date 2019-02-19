module Data.Witness.WitnessValue where

import Data.Kind

class WitnessValue (w :: k -> Type) where
    type WitnessValueType w :: Type
    witnessToValue :: forall (t :: k). w t -> WitnessValueType w
    valueToWitness :: forall r. WitnessValueType w -> (forall (t :: k). w t -> r) -> r
