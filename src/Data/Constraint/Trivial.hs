-- |
-- Module      :  Data.Constraint.Trivial
-- Copyright   :  (c) 2014-2016 Justus Sagemüller
-- License     :  GPL v3 (see LICENSE)
-- Maintainer  :  (@) jsagemue $ uni-koeln.de
--
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}

module Data.Constraint.Trivial (
            Unconstrained, Impossible
          , Unconstrained2, Impossible2
          , Unconstrained3, Impossible3
          , Unconstrained4, Impossible4
          , Unconstrained5, Impossible5
          , Unconstrained6, Impossible6
          , Unconstrained7, Impossible7
          , Unconstrained8, Impossible8
          , Unconstrained9, Impossible9
          , Disallowed(..)
          ) where

import           GHC.TypeLits
import           GHC.Exts (Any, TYPE)

class (Any, TypeError ('Text "All instances of "
          ':<>: 'Text t
          ':<>: 'Text " are disallowed.")) => Disallowed t where
  nope :: forall (a :: TYPE rep) => a

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
class Disallowed "Impossible" => Impossible t


-- | Like 'Unconstrained', but with kind signature @k -> k -> Constraint@
--   (two unconstrained types).
class Unconstrained2 t s
instance Unconstrained2 t s

class Disallowed "Impossible2" => Impossible2 t s


class Unconstrained3 t s r
instance Unconstrained3 t s r

class Disallowed "Impossible3" => Impossible3 t s r


class Unconstrained4 t s r q
instance Unconstrained4 t s r q

class Disallowed "Impossible4" => Impossible4 t s r q


class Unconstrained5 t s r q p
instance Unconstrained5 t s r q p

class Disallowed "Impossible5" => Impossible5 t s r q p


class Unconstrained6 t s r q p o
instance Unconstrained6 t s r q p o

class Disallowed "Impossible6" => Impossible6 t s r q p o


class Unconstrained7 t s r q p o n
instance Unconstrained7 t s r q p o n

class Disallowed "Impossible7" => Impossible7 t s r q p o n


class Unconstrained8 t s r q p o n m
instance Unconstrained8 t s r q p o n m

class Disallowed "Impossible8" => Impossible8 t s r q p o n m


class Unconstrained9 t s r q p o n m l
instance Unconstrained9 t s r q p o n m l

class Disallowed "Impossible9" => Impossible9 t s r q p o n m l
