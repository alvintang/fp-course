{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Monad where

import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P((=<<))

-- | All instances of the `Monad` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
-- f >=> (g >=> h) = (f >=> g) >=> h
-- pure >=> f = f = f >=> pure
class Applicative k => Monad k where
  -- Pronounced, bind.
  (=<<) ::
    (a -> k b)
    -> k a
    -> k b

infixr 1 =<<

-- | Binds a function on the ExactlyOne monad.
--
-- >>> (\x -> ExactlyOne(x+1)) =<< ExactlyOne 2
-- ExactlyOne 3
instance Monad ExactlyOne where
  (=<<) ::
    (a -> ExactlyOne b)
    -> ExactlyOne a
    -> ExactlyOne b
  (=<<) a2eb ea = a2eb (runExactlyOne ea)

    -- error "todo: Course.Monad (=<<)#instance ExactlyOne"

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Monad List where
  (=<<) ::
    (a -> List b)
    -> List a
    -> List b
  (=<<) a2lb la = case la of
    Nil -> Nil 
    a :. la' -> a2lb a ++ (a2lb =<< la')
    -- error "todo: Course.Monad (=<<)#instance List"

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Monad Optional where
  (=<<) ::
    (a -> Optional b)
    -> Optional a
    -> Optional b
  (=<<) a2ob oa = case oa of
    Full a -> a2ob a
    Empty -> Empty
    -- error "todo: Course.Monad (=<<)#instance Optional"

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Monad ((->) t) where
  (=<<) ::
    (a -> (t -> b))
    -> (t -> a)
    -> (t -> b)
    -- (a -> ((->) t b))
    -- -> ((->) t a)
    -- -> ((->) t b)
  (=<<) a2t2b t2a t = a2t2b (t2a t) t
    -- error "todo: Course.Monad (=<<)#instance ((->) t)"

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> ExactlyOne (+10) <**> ExactlyOne 8
-- ExactlyOne 18
--
-- >>> (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <**> Full 7
-- Full 15
--
-- >>> Empty <**> Full 7
-- Empty
--
-- >>> Full (+8) <**> Empty
-- Empty
--
-- >>> ((+) <**> (+10)) 3
-- 16
--
-- >>> ((+) <**> (+5)) 3
-- 11
--
-- >>> ((+) <**> (+5)) 1
-- 7
--
-- >>> ((*) <**> (+10)) 3
-- 39
--
-- >>> ((*) <**> (+2)) 3
-- 15
(<**>) ::
  Monad k =>
  k (a -> b)
  -> k a
  -> k b
(<**>) ka2b ka = ka2b <*> ka
  -- error "todo: Course.Monad#(<**>)"

infixl 4 <**>

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
-- (=<<) ::
--   (a -> k b)
--   -> k a
--   -> k b
-- a ~ k a
-- k a ~ k (k a)
-- k b ~ a

join ::
  Monad k =>
  k (k a)
  -> k a
join kka = id =<< kka
  -- error "todo: Course.Monad#join"

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
--  (<$>) ::
--     (a -> b) ~ a2kb :: (a -> k b)
--     -> k a
--     -> k b
  -- (<*>) ::
  --   k (a -> b)
  --   -> k a
  --   -> k b
(>>=) :: forall k a b.
  Monad k =>
  k a
  -> (a -> k b)
  -> k b
(>>=) ka a2kb = 
    -- join ((\a -> a2kb a) <$> ka)
    join (a2kb <$> ka)
  -- error "todo: Course.Monad#(>>=)"

infixl 1 >>=

-- | Implement composition within the @Monad@ environment.
-- Pronounced, Kleisli composition.
-- aka fish
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) ::
  Monad k =>
  (b -> k c)
  -> (a -> k b)
  -> a
  -> k c
(<=<) b2kc a2kb a = a2kb a >>= b2kc
  -- error "todo: Course.Monad#(<=<)"

infixr 1 <=<

-- (>>=) :: forall k a b.
--   Monad k =>
--   k a
--   -> (a -> k b)
--   -> k b
-- (>>=) ka a2kb = 
--     join (a2kb <$> ka)
ap :: 
  Monad m => 
  m (a -> b) 
  -> m a 
  -> m b
ap ma2b ma = ma >>= (\a -> ma2b >>= (\a2b -> pure (a2b a)))
-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where
  (=<<) =
    (P.=<<)
