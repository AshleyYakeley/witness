module Data.Witness.Representative where

import Data.Witness.Any
import Data.Witness.WitnessValue
import Import

isWitnessRepresentative :: Dict (Is rep a) -> rep a
isWitnessRepresentative Dict = representative

type Subrepresentative (p :: k -> Type) (q :: k -> Type) = forall (a :: k). p a -> Dict (Is q a)

withSubrepresentative :: Subrepresentative p q -> p a -> (Is q a => r) -> r
withSubrepresentative subrep pa f =
    case subrep pa of
        Dict -> f

type Representative :: forall k. (k -> Type) -> Constraint
class Representative rep where
    -- | Every value is an instance of 'Is'.
    getRepWitness :: Subrepresentative rep rep

instance Representative Proxy where
    getRepWitness Proxy = Dict

withRepresentative :: Representative rep => rep a -> (Is rep a => r) -> r
withRepresentative = withSubrepresentative getRepWitness

-- | If two representatives have the same type, then they have the same value.
type Is :: forall k. (k -> Type) -> k -> Constraint
class Representative rep => Is rep a where
    -- | The representative value for type @a@.
    representative :: rep a

instance Is Proxy a where
    representative = Proxy

getRepresentative :: Is rep a => a -> rep a
getRepresentative _ = representative

rerepresentative :: Is rep a => p a -> rep a
rerepresentative _ = representative

fromRepWitness :: Dict (Is rep a) -> rep a
fromRepWitness Dict = representative

mkAny :: Is rep a => a -> AnyValue rep
mkAny a = MkAnyValue representative a

mkAnyF :: Is rep a => f a -> AnyF rep f
mkAnyF fa = MkAnyF representative fa

instance Representative ((:~:) (t :: k)) where
    getRepWitness Refl = Dict

instance Is ((:~:) (t :: k)) (t :: k) where
    representative = Refl

instance Representative (Compose Dict c) where
    getRepWitness (Compose Dict) = Dict

instance c t => Is (Compose Dict c) t where
    representative = Compose Dict

typeValue ::
       forall k (rep :: k -> Type) (a :: k). (Is rep a, WitnessValue rep)
    => WitnessValueType rep
typeValue = witnessToValue @k @rep (representative @k @rep @a)
