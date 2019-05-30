-- |
-- Module      :  Data.Constraint.Trivial
-- Copyright   :  (c) 2014-2016 Justus Sagemüller
-- License     :  GPL v3 (see LICENSE)
-- Maintainer  :  (@) jsagemue $ uni-koeln.de
--
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Constraint.Trivial (
            Unconstrained, Impossible(..)
          , Unconstrained2, Impossible2(..)
          , Unconstrained3, Impossible3(..)
          , Unconstrained4, Impossible4(..)
          , Unconstrained5, Impossible5(..)
          , Unconstrained6, Impossible6(..)
          , Unconstrained7, Impossible7(..)
          , Unconstrained8, Impossible8(..)
          , Unconstrained9, Impossible9(..)
          ) where

import           GHC.TypeLits

type Disallowed t = 'Text "All instances of "
              ':<>: 'Text t
              ':<>: 'Text " are disallowed."

-- | Intended to be used as an argument for some type constructor which expects kind
--   @k -> Constraint@, when you do not actually wish to constrain anything with it.
--
--   @'Unconstrained' t@ can always be added to the constraint list of any signature, without
--   changing anything.
class Unconstrained t
instance Unconstrained t


-- | This constraint can /never/ be fulfilled. Might be useful e.g. as a default
--   for a class-associated constraint; this basically disables any method with
--   that constraint (so it can safely be left 'undefined').
class TypeError (Disallowed "Impossible") => Impossible t where
    -- | Ex falso quodlibet.  If you have @'Impossible' t@, you can do
    -- anything.
    --
    -- Might be useful e.g. if you are inside a function wiht an
    -- @'Impossible' t =>@ constraint, and you want to do something
    -- impossible, like the constraint implies.
    --
    -- Analogous to 'Data.Void.absurd'.
    absurdible :: proxy t -> a


-- | Like 'Unconstrained', but with kind signature @k -> k -> Constraint@
--   (two unconstrained types).
class Unconstrained2 t s
instance Unconstrained2 t s

class TypeError (Disallowed "Impossible2") => Impossible2 t s where
    absurdible2 :: proxy t -> proxy s -> a


class Unconstrained3 t s r
instance Unconstrained3 t s r

class TypeError (Disallowed "Impossible3") => Impossible3 t s r where
    absurdible3 :: proxy t -> proxy s -> proxy r -> a


class Unconstrained4 t s r q
instance Unconstrained4 t s r q

class TypeError (Disallowed "Impossible4") => Impossible4 t s r q where
    absurdible4 :: proxy t -> proxy s -> proxy r -> proxy q -> a


class Unconstrained5 t s r q p
instance Unconstrained5 t s r q p

class TypeError (Disallowed "Impossible5") => Impossible5 t s r q p where
    absurdible5 :: proxy t -> proxy s -> proxy r -> proxy q -> proxy p -> a


class Unconstrained6 t s r q p o
instance Unconstrained6 t s r q p o

class TypeError (Disallowed "Impossible6") => Impossible6 t s r q p o where
    absurdible6 :: proxy t -> proxy s -> proxy r
                -> proxy q -> proxy p -> proxy o
                -> a


class Unconstrained7 t s r q p o n
instance Unconstrained7 t s r q p o n

class TypeError (Disallowed "Impossible7") => Impossible7 t s r q p o n where
    absurdible7 :: proxy t -> proxy s -> proxy r -> proxy q
                -> proxy p -> proxy o -> proxy n -> a


class Unconstrained8 t s r q p o n m
instance Unconstrained8 t s r q p o n m

class TypeError (Disallowed "Impossible8") => Impossible8 t s r q p o n m where
    absurdible8 :: proxy t -> proxy s -> proxy r -> proxy q
                -> proxy p -> proxy o -> proxy n -> proxy m
                -> a


class Unconstrained9 t s r q p o n m l
instance Unconstrained9 t s r q p o n m l

class TypeError (Disallowed "Impossible9") => Impossible9 t s r q p o n m l where
    absurdible9 :: proxy t -> proxy s -> proxy r
                -> proxy q -> proxy p -> proxy o
                -> proxy n -> proxy m -> proxy l
                -> a
