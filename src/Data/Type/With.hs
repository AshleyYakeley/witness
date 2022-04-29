module Data.Type.With where

import Import

withRefl :: forall k (a :: k) (b :: k) (r :: Type). a :~: b -> (a ~ b => r) -> r
withRefl Refl r = r

reflCat ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k). Category cat
    => a :~: b -> cat a b
reflCat Refl = id

reflWitCat ::
       forall kq (cat :: kq -> kq -> Type) kp (w :: kp -> kq) (a :: kp) (b :: kp). Category cat
    => a :~: b -> cat (w a) (w b)
reflWitCat Refl = id
