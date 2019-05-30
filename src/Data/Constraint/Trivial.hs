-- |
-- Module      :  Data.Constraint.Trivial
-- Copyright   :  (c) 2014-2016 Justus Sagemüller
-- License     :  GPL v3 (see LICENSE)
-- Maintainer  :  (@) jsagemue $ uni-koeln.de
--
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

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
          ) where

import           GHC.Exts (Any)

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
--
--   This implementation works by requiring any instance also satify the
--   'Any' constraint, but the 'Any' constraint is impossible to ever
--   fulfil.
class Any => Impossible t


-- | Like 'Unconstrained', but with kind signature @k -> k -> Constraint@
--   (two unconstrained types).
class Unconstrained2 t s
instance Unconstrained2 t s

class Any => Impossible2 t s


class Unconstrained3 t s r
instance Unconstrained3 t s r

class Any => Impossible3 t s r


class Unconstrained4 t s r q
instance Unconstrained4 t s r q

class Any => Impossible4 t s r q


class Unconstrained5 t s r q p
instance Unconstrained5 t s r q p

class Any => Impossible5 t s r q p


class Unconstrained6 t s r q p o
instance Unconstrained6 t s r q p o

class Any => Impossible6 t s r q p o


class Unconstrained7 t s r q p o n
instance Unconstrained7 t s r q p o n

class Any => Impossible7 t s r q p o n


class Unconstrained8 t s r q p o n m
instance Unconstrained8 t s r q p o n m

class Any => Impossible8 t s r q p o n m


class Unconstrained9 t s r q p o n m l
instance Unconstrained9 t s r q p o n m l

class Any => Impossible9 t s r q p o n m l
