{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , FunctionalDependencies 
           , TypeFamilies
           , OverlappingInstances
           , UndecidableInstances 
           , EmptyDataDecls
           #-}

module Data.HList.Field.Base where

import Data.HList hiding (TypeEq, (#), tuple)
import Data.HList.Record
import Data.HList.Tuple

import Data.HList.Field.Label (Label(..), NestedLabel(..))
import Data.HList.Field.Utils

type EmptyRecord = Record HNil

infixr 8 <#, #
infixr 9 =:, =~

data HFHashT r l = HFHashT r l
data HFAssignT l r = HFAssignT l r
data HFModifyT l r = HFModifyT l r

(=:) = HFAssignT
(=~) = HFModifyT

-- |# preserves depth of nested labels
(#) :: (HFHash a b c) => a -> b -> c
(#) = hfHash

-- |<# extracts values (returns a tuple)
(<#) :: (HFApplyAndProject r f t) => r -> f -> t
(<#) = hfApplyAndProject

--------------------------------------------------
-- |HFHash -- implements (#)
--------------------------------------------------
class HFHash a b c | a b -> c where
  hfHash :: a -> b -> c
  
instance (IsTuple b f
         ,HFHash' f a b c
         ) => HFHash a b c where
  hfHash = hfHash' (undefined::f)
  
class HFHash' isTup a b c | isTup a b -> c where  
  hfHash' :: isTup -> a -> b -> c

instance (PrependLabel (Label l) b c
         ) => HFHash' HFalse (Label l) b c where
  hfHash' _ = prependLabel

instance (Untuple t tl
         ,PrependLabel (Label l) tl tl' 
         ,Tuple tl' t'
         ) => HFHash' HTrue (Label l) t t' where
  hfHash' _ l t = tuple $ prependLabel l $ untuple t

-- |Record # ...
instance (HFApply f (Record r) r' 
         ) => HFHash' isTup (Record r) f r' where
  hfHash' _ r f = hfApply f r
  
-- |... =: Record # ...
-- Handles setting fields by another hfield record construction.
-- Corrects the syntax error caused by the precedence rules
instance (HFApply f (Record r) v
         ) => HFHash' isTup (HFAssignT l1 (Record r)) f (HFAssignT l1 v) where
  hfHash' _ (HFAssignT l1 r) f = HFAssignT l1 $ hfApply f r

--------------------------------------------------
-- |Prepend Label
--------------------------------------------------
class PrependLabel a b c | a b -> c where
  prependLabel :: a -> b -> c
  
-- |HLists base case  
instance PrependLabel l HNil HNil where
  prependLabel _ _ = HNil
  
-- |HList recusive case
instance (PrependLabel (Label l) a a'
         ,PrependLabel (Label l) as as'
         ) => PrependLabel (Label l) (HCons a as) (HCons a' as') where
  prependLabel l (HCons a as) = HCons (prependLabel l a) (prependLabel l as)

-- |Label
instance PrependLabel (Label a) (Label b) (NestedLabel (HCons (Label a) (HCons (Label b) HNil))) where
  prependLabel a b = NestedLabel $ a .*. b .*. HNil
  
-- |NestedLabel  
instance (HExtend (Label l) ls ls'
         ) => PrependLabel (Label l) (NestedLabel ls) (NestedLabel ls') where
  prependLabel l (NestedLabel ls) = NestedLabel $ l .*. ls
  
-- |HFAssignT
instance (HFHash (Label l1) l2 l3
         ) => PrependLabel (Label l1) (HFAssignT l2 v) (HFAssignT l3 v) where
  prependLabel l1 (HFAssignT l2 v) = HFAssignT (l1 # l2) v

-- |HFModifyT
instance (HFHash (Label l1) l2 l3
         ) => PrependLabel (Label l1) (HFModifyT l2 v) (HFModifyT l3 v) where
  prependLabel l1 (HFModifyT l2 v) = HFModifyT (l1 # l2) v

--------------------------------------------------
-- |Dispatch by tag
--------------------------------------------------
class HFApply f r v | f r -> v where
  hfApply :: f -> r -> v
  
instance (IsTuple f tupP
         ,HFApply' tupP f r v
         ) => HFApply f r v where
  hfApply = hfApply' (undefined::tupP)

class HFApply' tupP f r v | tupP f r -> v where
  hfApply' :: tupP -> f -> r -> v
  
-- |Tuple
instance (Untuple fs fs'  
         ,HFApplyAll fs' r vs
         ) => HFApply' HTrue fs r vs where
  hfApply' _ fs r = hfApplyAll (untuple fs) r

-- |Label
instance (HFLookup (Label l) r v
         ) => HFApply' HFalse (Label l) r v where
  hfApply' _ l r = hfLookup l r  
  
-- |NestedLabel  
instance (HFLookup (NestedLabel ls) r v 
         ) => HFApply' HFalse (NestedLabel ls) r v where
  hfApply' _ ls r = hfLookup ls r
  
-- |Assign
instance (HFAssign l v r r' 
         ) => HFApply' HFalse (HFAssignT l v) r r' where
  hfApply' _ (HFAssignT l v) r = hfAssign l v r
  
-- |Modify
instance (HFModify l v v' r r' 
         ) => HFApply' HFalse (HFModifyT l (v -> v')) r r' where
  hfApply' _ (HFModifyT l f) r = hfModify l f r

--------------------------------------------------
-- |Apply multiple morphisms simultaneously
--------------------------------------------------  
class HFApplyAll fs r r' | fs r -> r' where
  hfApplyAll :: fs -> r -> r'
  
-- |apply multiple base case
instance HFApplyAll HNil r r where
  hfApplyAll _ r = r
  
-- |Recursive case
instance (HFApplyAll fs r r1
         ,HFApplyAll f r1 r2
         ) => HFApplyAll (HCons f fs) r r2 where
  hfApplyAll (HCons f fs) r 
    = let r1 = hfApplyAll fs r
      in hfApplyAll f r1
         
-- |Labels are no-op for ApplyAll  
instance HFApplyAll (Label l) r r where
  hfApplyAll _ r = r
  
-- |NestedLabels are no-op for ApplyAll
instance HFApplyAll (NestedLabel l) r r where
  hfApplyAll _ r = r
         
-- |Default is to delegate to hfApply                              
instance (HFApply f r r'
         ) => HFApplyAll f r r' where
  hfApplyAll f r = hfApply f r

--------------------------------------------------
-- |Lookup
--------------------------------------------------
class HFLookup f r v | f r -> v where
  hfLookup :: f -> r -> v
  
-- |Lookup by Label
instance (HasField (Label l) r v
         ) => HFLookup (Label l) r v where
  hfLookup l r = hLookupByLabel l r

-- |Lookup by NestLabel base case
instance HFLookup (NestedLabel HNil) v v where
  hfLookup _ v = v

-- |Lookup by NestLabel recursive case
instance (HasField l outer inner
         ,HFLookup (NestedLabel ls) inner v
         ) => HFLookup (NestedLabel (HCons l ls)) outer v where
  hfLookup (NestedLabel (HCons l ls)) outer
    = hfLookup (NestedLabel ls) inner
    where inner = hLookupByLabel l outer

  
--------------------------------------------------
-- |Lookup with default
--------------------------------------------------          
class HFLookupDefault l r d v | l r d -> v where
  hfLookupDefault :: l -> r -> d -> v
  
instance (RecordLabels r ls
         ,OccursP l ls occP
         ,HFLookupDefault' occP l r d v 
         ) => HFLookupDefault l r d v where
  hfLookupDefault = hfLookupDefault' (undefined::occP)
  
class HFLookupDefault' occP l r d v | occP l r d -> v where
  hfLookupDefault' :: occP -> l -> r -> d -> v
  
-- |Label does not occur.  Use default
instance HFLookupDefault' HFalse l r d d where
  hfLookupDefault' _ _ _ d = d
  
-- |Label does occur.  Lookup  
instance (HFLookup l r v
         ) => HFLookupDefault' HTrue l r d v where
  hfLookupDefault' _ l r _ = hfLookup l r

--------------------------------------------------          
-- Assign
--------------------------------------------------                  
class HFAssign l v r r' | l v r -> r' where
  hfAssign :: l -> v -> r -> r'

-- |Assign Label
instance (RecordLabels r ls
         ,OccursP (Label l) ls occP
         ,HFAssign' occP (Label l) v r r'
         ) => HFAssign (Label l) v r r' where
  hfAssign l v r = hfAssign' (undefined::occP) l v r
  
-- |Assign NestLabel base case  
instance HFAssign (NestedLabel HNil) v r v where
  hfAssign _ v _ = v
  
-- |Assign NestLabel recursive case
instance (HFLookupDefault l r (Record HNil) inner
         ,HFAssign (NestedLabel ls) v inner inner'
         ,HFAssign l inner' r r'
         ) => HFAssign (NestedLabel (HCons l ls)) v r r' where
  hfAssign (NestedLabel (HCons l ls)) v r
    = hfAssign l inner' r
    where inner = hfLookupDefault l r emptyRecord
          inner' = hfAssign (NestedLabel ls) v inner

class HFAssign' occP l v r r' | occP l v r -> r' where
  hfAssign' :: occP -> l -> v -> r -> r'
  
-- |Assign a new field
instance (HExtend (LVPair l v) r r'
         ) => HFAssign' HFalse l v r r' where
  hfAssign' _ l v r = (LVPair v) .*. r
  
-- |Update a field
instance (RecordLabels r ls
         ,HFind l ls n
         ,HUpdateAtHNat n (LVPair l v) r r'
         ) => HFAssign' HTrue l v (Record r) (Record r') where
  hfAssign' _ = hUpdateAtLabel 

--------------------------------------------------
-- |Modify
--------------------------------------------------  
class HFModify l v v' r r' | l v v' r -> r' where
  hfModify :: l -> (v -> v') -> r -> r'
  
-- |Modify Label
instance (HFLookup l r v1
         ,HFAssign l v1' r r'
         ,v ~ v1
         ,v' ~ v1'
         ) => HFModify l v v' r r' where
  hfModify l f r 
    = hfAssign l v' r
    where v = hfLookup l r
          v' = f v

--------------------------------------------------
-- |Update
--------------------------------------------------
data HFUpdateT

class HFUpdate r1 r2 r3 | r1 r2 -> r3 where
  hfUpdate :: r1 -> r2 -> r3  
  
instance (HFFold HFUpdateT r1 r2 r3
         ) => HFUpdate r1 r2 r3 where
  hfUpdate = hfFold (undefined::HFUpdateT)

instance (HFAssign l v s s'
         ) => HFFoldStep HFUpdateT s (LVPair l v) s' where
  hfFoldStep _ r (LVPair v) = hfAssign (undefined::l) v r
          
--------------------------------------------------          
-- |Union:  safe update; label collisions cause error
--------------------------------------------------          
data HFUnionT

class HFUnion r1 r2 r3 | r1 r2 -> r3 where
  hfUnion :: r1 -> r2 -> r3
  
instance (HFFold HFUnionT r1 r2 r3
         ) => HFUnion r1 r2 r3 where
  hfUnion = hfFold (undefined::HFUnionT)

instance (HExtend e s s'
         ) => HFFoldStep HFUnionT s e s' where
  hfFoldStep _ s e = e .*. s

--------------------------------------------------
-- |Fold:  update/union implementation
--------------------------------------------------
class HFFold m s es s' | m s es -> s' where
  hfFold :: m -> s -> es -> s'

class HFFoldStep m s e s' | m s e -> s' where
  hfFoldStep :: m -> s -> e -> s'
  
instance HFFold m s HNil s where
  hfFold _ s _ = s
  
instance (HFFold m s es s'
         ) => HFFold m s (Record es) s' where
  hfFold m s (Record es) = hfFold m s es
  
instance (HFFold m s es s'
         ,HFFoldStep m s' e s''
         ) => HFFold m s (HCons e es) s'' where
  hfFold m s (HCons e es) 
    = hfFoldStep m s' e
    where s' = hfFold m s es
          
--------------------------------------------------
-- |Apply and Project
--------------------------------------------------
class HFApplyAndProject r f t | r f -> t where
  hfApplyAndProject :: r -> f -> t
  
instance (HFApplyAll f r r'
         ,HFLabels f ls
         ,HFProject ls r' t
         ) => HFApplyAndProject r f t where
  hfApplyAndProject r f 
    = let r2 = hfApplyAll f r
          ls = hfLabels f
      in hfProject ls r2

--------------------------------------------------
-- |Get the labels from an HF operation
--------------------------------------------------
class HFLabels f ls | f -> ls where
  hfLabels :: f -> ls
  
instance (IsTuple f tupP
         ,HFLabels' tupP f ls
         ) => HFLabels f ls where
  hfLabels = hfLabels' (undefined::tupP)
  
class HFLabels' tupP f ls | tupP f -> ls where
  hfLabels' :: tupP -> f -> ls
  
-- |Tuple case: convert to HList
instance (Untuple f fs'
         ,HFLabels fs' ls
         ) => HFLabels' HTrue f ls where
  hfLabels' _ f = hfLabels $ untuple f
  
-- |HList base case  
instance HFLabels' HFalse HNil HNil where
  hfLabels' _ _ = HNil
  
-- |HList recursive case
instance (HFLabels f ls
         ,HFLabels fs ls'
         ,HAppend ls ls' ls''
         ) => HFLabels' HFalse (HCons f fs) ls'' where
  hfLabels' _ (HCons f fs) 
    = let ls = hfLabels f
          ls' = hfLabels fs
      in hAppend ls ls'
      
instance HFLabels' HFalse (Label l) (HCons (Label l) HNil) where
  hfLabels' _ l = l .*. HNil
  
instance HFLabels' HFalse (NestedLabel ls) (HCons (NestedLabel ls) HNil) where
  hfLabels' _ ls = ls .*. HNil
  
instance HFLabels' HFalse (HFAssignT l v) (HCons l HNil) where
  hfLabels' _ (HFAssignT l _) = l .*. HNil
  
instance HFLabels' HFalse (HFModifyT l f) (HCons l HNil) where
  hfLabels' _ (HFModifyT l _) = l .*. HNil
  
-- |Single element default: recurse
instance (HFLabels f ls
         ) => HFLabels' HFalse f ls where
  hfLabels' _ f 
    = hfLabels f

--------------------------------------------------
-- |Project labels into a tuple
--------------------------------------------------
class HFProject ls r t | ls r -> t where
  hfProject :: ls -> r -> t
  
instance (HFProject' ls r vs
         ,Tuple vs t
         ) => HFProject ls r t where
  hfProject ls r = tuple $ hfProject' ls r
  
class HFProject' ls r vs | ls r -> vs where
  hfProject' :: ls -> r -> vs
  
instance HFProject' HNil r HNil where
  hfProject' _ _ = HNil
  
instance (HFLookup l r v
         ,HFProject' ls r vs
         ,HExtend v vs vs'
         ) => HFProject' (HCons l ls) r vs' where
  hfProject' (HCons l ls) r 
    = let v = hfLookup l r
          vs = hfProject' ls r 
      in v .*. vs

