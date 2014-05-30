-- |
-- Module      :  Data.Constraint.Trivial
-- Copyright   :  (c) 2014 Justus Sagemüller
-- License     :  GPL v3 (see LICENSE)
-- Maintainer  :  (@) sagemuej $ smail.uni-koeln.de
-- 
{-# LANGUAGE FlexibleInstances #-}

module Data.Constraint.Trivial where

-- | Intended to be used as an argument for some type constructor which expects kind
--   @* -> Constraint@, when you do not actually wish to constrain anything with it.
--   
--   @'Trivial' t@ can always be added to the constraint list of any signature, without
--   changing anything.
class Trivial t
instance Trivial t
