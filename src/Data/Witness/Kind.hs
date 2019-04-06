module Data.Witness.Kind where

import Data.Constraint
import Data.Kind
import Data.Witness.Representative

type family KindWitness k :: k -> Type

type InKind (a :: k) = Is (KindWitness k) a

inKind ::
       forall k (a :: k). InKind a
    => KindWitness k a
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
    MkFunctionKindWitness
        :: forall kp kq (wp :: kp -> Type) (wq :: kq -> Type) (t :: kp -> kq).
           (forall (p :: kp). Is wp p => Is wq (t p))
        => FunctionKindWitness wp wq t

applyFunctionKindWitness ::
       forall kp kq (wp :: kp -> Type) (wq :: kq -> Type) (t :: kp -> kq) (p :: kp) (proxy :: kp -> Type). Is wp p
    => FunctionKindWitness wp wq t
    -> proxy p
    -> Dict (Is wq (t p))
applyFunctionKindWitness MkFunctionKindWitness _ = Dict

type instance KindWitness (kp -> kq) =
     FunctionKindWitness (KindWitness kp) (KindWitness kq)

instance forall kp kq (wp :: kp -> Type) (wq :: kq -> Type). Representative (FunctionKindWitness wp wq) {- Representative wq => -}
                                                                                                                                   where
    getRepWitness MkFunctionKindWitness = Dict

instance (forall p. Is wp p => Is wq (f p)) =>
             Is (FunctionKindWitness (wp :: kp -> Type) (wq :: kq -> Type)) (f :: kp -> kq) where
    representative = MkFunctionKindWitness

-- (kp,kq)
data PairWitness (wp :: kp -> Type) (wq :: kq -> Type) (t :: (kp, kq)) where
    MkPairWitness :: (Is wp p, Is wq q) => PairWitness wp wq '( p, q)

instance (Representative wp, Representative wq) => Representative (PairWitness wp wq) where
    getRepWitness MkPairWitness = Dict

instance (Is wp p, Is wq q) => Is (PairWitness wp wq) '( p, q) where
    representative = MkPairWitness

type instance KindWitness (kp, kq) =
     PairWitness (KindWitness kp) (KindWitness kq)

data AnyInKind (wit :: k -> Type) =
    forall (t :: k). InKind t => MkAnyInKind (wit t)
