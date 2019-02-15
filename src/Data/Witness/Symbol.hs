module Data.Witness.Symbol
    ( module Data.Witness.Symbol
    , Symbol
    , KnownSymbol
    ) where

import Data.Constraint
import Data.Proxy
import Data.Type.Equality
import Data.Witness.Constraint
import GHC.TypeLits
import Prelude

data SymbolType (symbol :: Symbol) where
    MkSymbolType :: KnownSymbol symbol => SymbolType symbol

fromSymbolType :: forall (symbol :: Symbol). SymbolType symbol -> String
fromSymbolType MkSymbolType = symbolVal (Proxy :: Proxy symbol)

toSymbolType :: String -> (forall (symbol :: Symbol). SymbolType symbol -> r) -> r
toSymbolType s cont =
    case someSymbolVal s of
        SomeSymbol p -> let
            psw :: forall (symbol :: Symbol). KnownSymbol symbol
                => Proxy symbol
                -> SymbolType symbol
            psw _ = MkSymbolType
            in cont $ psw p

instance TestEquality SymbolType where
    testEquality (MkSymbolType :: SymbolType a) (MkSymbolType :: SymbolType b) = sameSymbol (Proxy @a) (Proxy @b)

instance Show (SymbolType symbol) where
    show = fromSymbolType

instance AllWitnessConstraint Show SymbolType where
    allWitnessConstraint = Dict
