module Data.Witness.BigNat
    ( module Data.Witness.BigNat
    , KnownNat
    ) where

import Data.Constraint
import Data.Kind
import Data.Proxy
import Data.Type.Equality
import Data.Witness.Constraint
import Data.Witness.Representative
import Data.Witness.WitnessValue
import GHC.TypeLits
import Numeric.Natural
import Prelude

type BigNat = Nat

type BigNatType :: BigNat -> Type
data BigNatType bn where
    MkBigNatType :: KnownNat bn => BigNatType bn

instance WitnessValue BigNatType where
    type WitnessValueType BigNatType = Natural
    witnessToValue :: forall t. BigNatType t -> Natural
    witnessToValue MkBigNatType = fromInteger $ natVal (Proxy :: Proxy t)
    valueToWitness i cont =
        case someNatVal $ toInteger i of
            Just (SomeNat p) -> let
                psw :: forall (t :: BigNat). KnownNat t
                    => Proxy t
                    -> BigNatType t
                psw _ = MkBigNatType
                in cont $ psw p
            Nothing -> error "negative Natural"

instance TestEquality BigNatType where
    testEquality (MkBigNatType :: BigNatType a) (MkBigNatType :: BigNatType b) = sameNat (Proxy @a) (Proxy @b)

instance Representative BigNatType where
    getRepWitness MkBigNatType = Dict

instance KnownNat bn => Is BigNatType bn where
    representative = MkBigNatType

instance Show (BigNatType bn) where
    show = show . witnessToValue

instance AllWitnessConstraint Show BigNatType where
    allWitnessConstraint = Dict
