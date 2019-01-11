module Data.Witness.Representative where

import Data.Constraint
import Data.Kind
import Data.Proxy
import Data.Witness.Any

isWitnessRepresentative :: Dict (Is rep a) -> rep a
isWitnessRepresentative Dict = representative

type Subrepresentative (p :: k -> Type) (q :: k -> Type) = forall (a :: k). p a -> Dict (Is q a)

withSubrepresentative :: Subrepresentative p q -> p a -> (Is q a => r) -> r
withSubrepresentative subrep pa f =
    case subrep pa of
        Dict -> f

class Representative (rep :: k -> Type) where
    -- | Every value is an instance of 'Is'.
    getRepWitness :: Subrepresentative rep rep

instance Representative Proxy where
    getRepWitness Proxy = Dict

withRepresentative :: Representative rep => rep a -> (Is rep a => r) -> r
withRepresentative = withSubrepresentative getRepWitness

-- | If two representatives have the same type, then they have the same value.
class Representative rep => Is (rep :: k -> Type) (a :: k) where
    -- | The representative value for type @a@.
    representative :: rep a

instance Is Proxy a where
    representative = Proxy

getRepresentative :: (Is rep a) => a -> rep a
getRepresentative _ = representative

rerepresentative :: (Is rep a) => p a -> rep a
rerepresentative _ = representative

mkAny :: (Is rep a) => a -> AnyValue rep
mkAny a = MkAnyValue representative a

mkAnyF :: (Is rep a) => f a -> AnyF rep f
mkAnyF fa = MkAnyF representative fa
