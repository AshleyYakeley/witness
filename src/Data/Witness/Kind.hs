module Data.Witness.Kind where

import Data.Constraint
import Data.Kind
import Data.Witness.Representative

type family KindWitness k :: k -> Type

type InKind (a :: k) = Is (KindWitness k) a

inKind :: InKind a => KindWitness k a
inKind = representative

-- NoWitness
data NoWitness (t :: k) =
    MkNoWitness

instance Representative NoWitness where
    getRepWitness MkNoWitness = Dict

instance Is NoWitness a where
    representative = MkNoWitness

-- Type
type instance KindWitness Type = NoWitness

-- Constraint
type instance KindWitness Constraint = NoWitness

-- kp -> kq
data FunctionKindWitness (wp :: kp -> Type) (wq :: kq -> Type) (t :: kp -> kq) where
    MkFunctionKindWitness :: (forall (p :: kp). wp p -> wq (t p)) -> FunctionKindWitness wp wq t

{-
instance (forall p. Is wp p => Is wq (f p)) => Is (FunctionKindWitness wp wq) f where
    representative = MkFunctionKindWitness $ \wit -> case getRepWitness wit of
        Dict -> Dict
-}
type instance KindWitness (kp -> kq) =
     FunctionKindWitness (KindWitness kp) (KindWitness kq)

-- (kp,kq)
data PairWitness (wp :: kp -> Type) (wq :: kq -> Type) (t :: (kp, kq)) where
    MkPairWitness :: wp p -> wq q -> PairWitness wp wq '( p, q)

instance (Representative wp, Representative wq) => Representative (PairWitness wp wq) where
    getRepWitness (MkPairWitness wp wq) =
        case (getRepWitness wp, getRepWitness wq) of
            (Dict, Dict) -> Dict

instance (Is wp p, Is wq q) => Is (PairWitness wp wq) '( p, q) where
    representative = MkPairWitness representative representative

type instance KindWitness (kp, kq) =
     PairWitness (KindWitness kp) (KindWitness kq)
