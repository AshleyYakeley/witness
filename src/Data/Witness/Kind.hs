module Data.Witness.Kind where

import Data.Constraint
import Data.Kind
import Data.Witness.Constraint
import Data.Witness.Representative
import GHC.TypeLits
import Prelude

type KindWitness :: forall k -> k -> Type
type family KindWitness k

type InKind (a :: k) = Is (KindWitness k) a

inKind ::
       forall k (a :: k). InKind a
    => KindWitness k a
inKind = representative

-- NoWitness
type NoWitness :: forall k. k -> Type
data NoWitness t =
    MkNoWitness

instance Representative NoWitness where
    getRepWitness MkNoWitness = Dict

instance Is NoWitness a where
    representative = MkNoWitness

-- Type
type instance KindWitness Type = NoWitness

-- Constraint
type instance KindWitness Constraint = NoWitness

-- Nat
type instance KindWitness Nat = NoWitness

-- kp -> kq
type FunctionKindWitness :: forall kp kq. (kp -> Type) -> (kq -> Type) -> ((kp -> kq) -> Type)
data FunctionKindWitness wp wq t where
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

deriveIsFunctionKindWitness ::
       forall kp kq (wp :: kp -> Type) (wq :: kq -> Type) (f :: kp -> kq). (forall p. Is wp p => Is wq (f p))
    => Dict (Is (FunctionKindWitness wp wq) f)
deriveIsFunctionKindWitness = Dict

-- (kp,kq)
type PairWitness :: forall kp kq. (kp -> Type) -> (kq -> Type) -> ((kp, kq) -> Type)
data PairWitness wp wq t where
    MkPairWitness :: (Is wp p, Is wq q) => PairWitness wp wq '( p, q)

instance (Representative wp, Representative wq) => Representative (PairWitness wp wq) where
    getRepWitness MkPairWitness = Dict

instance (Is wp p, Is wq q) => Is (PairWitness wp wq) '( p, q) where
    representative = MkPairWitness

type instance KindWitness (kp, kq) =
     PairWitness (KindWitness kp) (KindWitness kq)

type AnyInKind :: forall k. (k -> Type) -> Type
data AnyInKind wit =
    forall t. InKind t => MkAnyInKind (wit t)

instance AllWitnessConstraint Show w => Show (AnyInKind w) where
    show (MkAnyInKind wa) = showAllWitness wa

-- AllInKind
type AllInKind :: Type -> Constraint
class AllInKind k where
    withAllInKind ::
           forall kw r. (kw ~ KindWitness k)
        => ((forall (a :: k). Is kw a) => r)
        -> r

allInKind ::
       forall k (a :: k). AllInKind k
    => Dict (InKind a)
allInKind = let
    helper ::
           forall kw. kw ~ KindWitness k
        => Dict (Is kw a)
    helper = withAllInKind @k Dict
    in helper

instance AllInKind Type where
    withAllInKind r = r

instance AllInKind Constraint where
    withAllInKind r = r

instance AllInKind kq => AllInKind (kp -> kq) where
    withAllInKind r = withAllInKind @kq r
