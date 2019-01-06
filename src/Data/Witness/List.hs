module Data.Witness.List where

import Data.Constraint (Dict(..))
import Data.Nat
import Data.Proxy
import Data.Type.Equality
import Data.Witness.Representative
import Prelude hiding ((.), id)

-- | a witness type for lists of types
-- The @w@ parameter is the witness type of the elements.
data ListType (w :: k -> *) (lt :: [k]) where
    NilListType :: ListType w '[]
    ConsListType :: w a -> ListType w lt -> ListType w (a : lt)

instance (Representative w) => Representative (ListType w) where
    getRepWitness NilListType = Dict
    getRepWitness (ConsListType w lw) =
        case (getRepWitness w, getRepWitness lw) of
            (Dict, Dict) -> Dict

instance (Representative w) => Is (ListType w) '[] where
    representative = NilListType

instance (Is w a, Is (ListType w) lt) => Is (ListType w) (a : lt) where
    representative = ConsListType representative representative

instance (TestEquality w) => TestEquality (ListType w) where
    testEquality NilListType NilListType = Just Refl
    testEquality (ConsListType wpa wpb) (ConsListType wqa wqb) = do
        Refl <- testEquality wpa wqa
        Refl <- testEquality wpb wqb
        return Refl
    testEquality _ _ = Nothing

class KnownList (t :: [k]) where
    listType :: ListType Proxy t

instance KnownList '[] where
    listType = NilListType

instance KnownList lt => KnownList (a : lt) where
    listType = ConsListType Proxy listType

type family ListElement (n :: Nat) (list :: [k]) :: k where
    ListElement 'Zero (a : aa) = a
    ListElement ('Succ n) (a : aa) = ListElement n aa
