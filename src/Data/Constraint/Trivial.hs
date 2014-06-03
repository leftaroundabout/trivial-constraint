-- |
-- Module      :  Data.Constraint.Trivial
-- Copyright   :  (c) 2014 Justus Sagemüller
-- License     :  GPL v3 (see LICENSE)
-- Maintainer  :  (@) sagemuej $ smail.uni-koeln.de
-- 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds   #-}

module Data.Constraint.Trivial (Unconstrained, Impossible) where

-- | Intended to be used as an argument for some type constructor which expects kind
--   @* -> Constraint@, when you do not actually wish to constrain anything with it.
--   
--   @'Unconstrained' t@ can always be added to the constraint list of any signature, without
--   changing anything.
class Unconstrained t
instance Unconstrained t


-- | This constraint can /never/ be fulfilled. Might be useful e.g. as a default
--   for a class-associated constraint; this basically disables any method with
--   that constraint (so it can safely be left 'undefined').
type Impossible t = HiddenEmptyClass t

class HiddenEmptyClass t

