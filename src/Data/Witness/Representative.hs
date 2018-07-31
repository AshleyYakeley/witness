module Data.Witness.Representative where

import Data.Constraint
import Data.Kind
import Data.Proxy
import Data.Witness.Any

isWitnessRepresentative :: Dict (Is rep a) -> rep a
isWitnessRepresentative Dict = representative

class Representative (rep :: k -> *) where
    -- | Every value is an instance of 'Is'.
    getRepWitness :: forall (a :: k). rep a -> Dict (Is rep a)

instance Representative Proxy where
    getRepWitness Proxy = Dict

withRepresentative ::
       forall (rep :: k -> *) r. (Representative rep)
    => (forall (a :: k). (Is rep a) => rep a -> r)
    -> (forall (b :: k). rep b -> r)
withRepresentative foo rep =
    case getRepWitness rep of
        Dict -> foo rep

-- | If two representatives have the same type, then they have the same value.
class Representative rep => Is (rep :: k -> *) (a :: k) where
    -- | The representative value for type @a@.
    representative :: rep a

instance Is Proxy a where
    representative = Proxy

getRepresentative :: (Is rep a) => a -> rep a
getRepresentative _ = representative

rerepresentative :: (Is rep a) => p a -> rep a
rerepresentative _ = representative

mkAny :: (Is rep a) => a -> Any rep
mkAny a = MkAny representative a

mkAnyF :: (Is rep a) => f a -> AnyF rep f
mkAnyF fa = MkAnyF representative fa
