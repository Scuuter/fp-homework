{-# LANGUAGE RankNTypes #-}

module BaseLenses (
    set
  , view
  , over
  , (.~)
  , (^.)
  , (%~)
  , _1
  , _2
  , lens
  , lens'
  , choosing
  , (<%~)
  , (<<%~)
) where

import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Functor.Const (Const(..), getConst)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type Lens' s a  = Lens s s a a


set  :: Lens' s a -> a -> s -> s         -- set    value (setter)
set lns a s = runIdentity $ lns (Identity . const a) s

view :: Lens' s a -> s -> a              -- lookup value (getter)
view lns s = getConst $ lns Const s

over :: Lens' s a -> (a -> a) -> s -> s  -- change value (modifier)
over lns fun s = runIdentity $ lns (Identity . fun) s

(.~) :: Lens s t a b -> b -> s -> t
(.~) lns a s = runIdentity $ lns (Identity . const a) s

(^.) :: s -> Lens s t a b -> a
(^.) s lns = getConst $ lns Const s

(%~) :: Lens s t a b -> (a -> b) -> s -> t
(%~) lns fun s = runIdentity $ lns (Identity . fun) s

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 fun (a, x) = (\b -> (b, x)) <$> fun a
-- (\b -> (b, x)) <$> f b = f (b, x)

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 fun (x, a) = (\b -> (x, b)) <$> fun a

lens' :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens' get set' = \f s -> set' s <$> f (get s)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set' = \f s -> set' s <$> f (get s)


choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = \f s -> case s of
  Left a -> Left <$> l1 f a
  Right a -> Right <$> l2 f a


-- Изменить цель линзы и вернуть новый результат.
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (f $ s ^. l, l %~ f $ s)

-- Изменить цель линзы, но вернуть старый результат.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (s ^. l, l %~ f $ s)
