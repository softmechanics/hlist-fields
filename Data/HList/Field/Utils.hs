{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , FunctionalDependencies 
           , TypeFamilies
           , OverlappingInstances
           , UndecidableInstances #-}

module Data.HList.Field.Utils where

import Data.HList hiding (TypeEq)
import Data.HList.FakePrelude (TypeCast(..))
import Data.HList.TypeEqGeneric2

instance (RecordLabels r ls) => RecordLabels (Record r) ls
instance (TypeEq a b f, HBool f) => HEq a b f

-- |Type-level occurs predicate.  
-- HTrue if type x is an element of HList xs
class OccursP x xs occ | x xs -> occ
instance OccursP x HNil HFalse
instance (HEq x x' eq
         ,OccursP' eq x xs occ
         ) => OccursP x (HCons x' xs) occ
instance (RecordLabels (Record r) ls
         ,OccursP l ls occ
         ) => OccursP x (Record r) occ

class OccursP' eq x xs occ | eq x xs -> occ
instance OccursP' HTrue x xs HTrue
instance (OccursP x xs occ) => OccursP' HFalse x xs occ 

-- |Type-level record predicate 
class RecordP a f | a -> f
instance TypeCast flag HFalse => RecordP a flag
instance RecordP (Record r) HTrue

