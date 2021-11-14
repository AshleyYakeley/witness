module Data.Witness.Symbol
    ( module Data.Witness.Symbol
    , Symbol
    , KnownSymbol
    ) where

import Data.Constraint
import Data.Kind
import Data.Proxy
import Data.Type.Equality
import Data.Witness.Constraint
import Data.Witness.Representative
import Data.Witness.WitnessValue
import GHC.TypeLits
import Prelude

type SymbolType :: Symbol -> Type
data SymbolType symbol where
    MkSymbolType :: KnownSymbol symbol => SymbolType symbol

instance WitnessValue SymbolType where
    type WitnessValueType SymbolType = String
    witnessToValue :: forall t. SymbolType t -> String
    witnessToValue MkSymbolType = symbolVal (Proxy :: Proxy t)
    valueToWitness s cont =
        case someSymbolVal s of
            SomeSymbol p -> let
                psw :: forall (t :: Symbol). KnownSymbol t
                    => Proxy t
                    -> SymbolType t
                psw _ = MkSymbolType
                in cont $ psw p

instance TestEquality SymbolType where
    testEquality (MkSymbolType :: SymbolType a) (MkSymbolType :: SymbolType b) = sameSymbol (Proxy @a) (Proxy @b)

instance Representative SymbolType where
    getRepWitness MkSymbolType = Dict

instance KnownSymbol symbol => Is SymbolType symbol where
    representative = MkSymbolType

instance Show (SymbolType symbol) where
    show = witnessToValue

instance AllWitnessConstraint Show SymbolType where
    allWitnessConstraint = Dict
