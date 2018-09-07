module Data.Witness.Symbol where

import Data.Constraint
import Data.Proxy
import Data.Type.Equality
import Data.Witness.Constraint
import GHC.TypeLits
import Prelude

data SymbolWitness (symbol :: Symbol) where
    MkSymbolWitness :: KnownSymbol symbol => SymbolWitness symbol

fromSymbolWitness :: forall (symbol :: Symbol). SymbolWitness symbol -> String
fromSymbolWitness MkSymbolWitness = symbolVal (Proxy :: Proxy symbol)

toSymbolWitness :: String -> (forall (symbol :: Symbol). SymbolWitness symbol -> r) -> r
toSymbolWitness s cont =
    case someSymbolVal s of
        SomeSymbol p -> let
            psw :: forall (symbol :: Symbol). KnownSymbol symbol
                => Proxy symbol
                -> SymbolWitness symbol
            psw _ = MkSymbolWitness
            in cont $ psw p

instance TestEquality SymbolWitness where
    testEquality (MkSymbolWitness :: SymbolWitness a) (MkSymbolWitness :: SymbolWitness b) =
        sameSymbol (Proxy @a) (Proxy @b)

instance Show (SymbolWitness symbol) where
    show = fromSymbolWitness

instance AllWitnessConstraint Show SymbolWitness where
    allWitnessConstraint = Dict
