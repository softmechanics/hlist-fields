{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , FunctionalDependencies 
           , TypeFamilies
           , OverlappingInstances
           , UndecidableInstances #-}

module Data.HList.Field.Base (
  HLookup(..), 
  HLookup'(..),
  HApplyAll(..),
  DotHash(..),
  DotHash'(..),
  (=:), (=~), (.#), (#),

  HProjectByFields(..),
  NestLabels(..),
  TypeEq(..)
  ) where

import Data.HList.Record
import Data.HList.Tuple
import Data.HList hiding (TypeEq, (#))
import Data.HList.TypeEqGeneric2
import Data.HList.Field.Label (Label(..))

instance (TypeEq a b f, HBool f) => HEq a b f

infixr 7 .# 
infixr 8 =:, =~
infixl 9 #

data EqualColon l r = EqualColon l r
data EqualTilde l r = EqualTilde l r

(=:) = EqualColon
(=~) = EqualTilde

newtype NestLabels a = NestLabels a
type EmptyRecord = Record HNil

(.#) :: DotHash a b c => a -> b -> c
(.#) = dotHash

(#) :: HLookup a b c => a -> b -> c
(#) = hLookup

class DotHash a b c | a b -> c where
  dotHash :: a -> b -> c

instance (IsTuple b f
         ,DotHash' f a b c
         ) => DotHash a b c where
  dotHash a b = dotHash' (undefined::f) a b

class DotHash' f a b c | f a b -> c where
  dotHash' :: f -> a -> b -> c

instance (Untuple fs fs'
         ,HApplyAll fs' (Record r) (Record r')
         ) => DotHash' HTrue (Record r) fs (Record r') where
  dotHash' _ r fs = hApplyAll (untuple fs) r

instance (HUpdateAtHNat n (LVPair (Label l) (Record inner')) out out'
         ,HFind (Label l) ls n
         ,RecordLabels out ls
         ,HasField (Label l) out (Record inner)
         ,Untuple fs fs'
         ,HApplyAll fs' (Record inner) (Record inner')
         ) => DotHash' HTrue (Label l) fs (Record out -> Record out') where
  dotHash' _ l fs = \out -> let inner = out # l 
                                inner' = hApplyAll (untuple fs) inner
                            in hUpdateAtLabel l inner' out


instance (HApply f (Record fr) (Record fr')
         ,r ~ fr
         ,r' ~ fr'
         ) => DotHash' HFalse (Record r) f (Record r') where
  dotHash' _ rec f = hApply f rec


instance (HUpdateAtHNat n (LVPair (Label l) inner') out out'
         ,HFind (Label l) ls n
         ,RecordLabels out ls
         ,HasField (Label l) out inner
         ,HApply f fInner fInner'
         ,inner ~ fInner
         ,inner' ~ fInner'
         ) => DotHash' HFalse (Label l) f (Record out -> Record out') where
  dotHash' _ l f = \out -> let inner = out # l 
                           in hUpdateAtLabel l (hApply f inner) out

class HApply f r r' | f r -> r' where
  hApply :: f -> r -> r'

instance (fr ~ r, fr' ~ r') => HApply (fr -> fr') r r' where
  hApply f r = f r

instance (AddOrUpdateLabel l v cols cols'
         ) => HApply (EqualColon l v) cols cols' where
  hApply (EqualColon l v) r = addOrUpdateLabel l v r

instance (HUpdateAtHNat n (LVPair l v') cols cols'
         ,HFind l ls n
         ,RecordLabels cols ls
         ,HasField l (Record cols) v
         ,HLookup (Record cols) l v
         ) => HApply (EqualTilde l (v -> v')) (Record cols) (Record cols') where
  hApply (EqualTilde l f) r = hUpdateAtLabel l (f $ r # l) r

class HApplyAll fs r r' | fs r -> r' where
  hApplyAll :: fs -> r -> r'

instance HApplyAll HNil r r where
  hApplyAll _ r = r

instance (HApplyAll fs fsR fsR'
         ,HApply f fR fR'
         ,pR ~ fsR
         ,fsR' ~ fR
         ,fR' ~ pR'
         ) =>  HApplyAll (HCons f fs) pR pR' where
  hApplyAll (HCons f fs) r = hApply f $ hApplyAll fs r

class HLookup a b c | a b -> c where
  hLookup :: a -> b -> c

instance (IsTuple l f
         ,HLookup' f r l r'
         ) => HLookup r l r' where
  hLookup = hLookup' (undefined::f)
  
class HLookup' isTup a b c | isTup a b -> c where
  hLookup' :: isTup -> a -> b -> c

instance (HasField (Label l) (Record r) v
         ) => HLookup' HFalse (Record r) (Label l) v where
  hLookup' _ r l = hLookupByLabel l r
  
instance (HProjectByFields (HCons l ls) (Record r) (Record r')
         ) => HLookup' HFalse (Record r) (HCons l ls) (Record r') where
  hLookup' _ r ls = hProjectByFields ls r

instance HLookup' HFalse (Label l1) (Label l2) (NestLabels (HCons (Label l1) (HCons (Label l2) HNil))) where
  hLookup' _ l1 l2 = NestLabels $ l1 .*. l2 .*. HNil
  
instance (IsTuple l f
         ,HLookup' f (Record r) l v
         ) => HLookup' HFalse (Record r) (NestLabels (HCons l HNil)) v where
  hLookup' _ r (NestLabels (HCons l HNil)) = hLookup' (undefined::f) r l
  
instance (IsTuple (NestLabels (HCons l' ls)) f
         ,HLookup' f (Record out) l (Record inner)
         ,IsTuple l f'
         ,HLookup' f' (Record inner) (NestLabels (HCons l' ls)) v
         ) => HLookup' HFalse (Record out) (NestLabels (HCons l (HCons l' ls))) v where
  hLookup' _ r (NestLabels (HCons l ls))
    = let inner = hLookup' (undefined::f) r l
      in hLookup' (undefined::f') inner (NestLabels ls)

instance (Untuple t hl
         ,HLookup (Record r) hl r'
         ) => HLookup' HTrue (Record r) t r' where
  hLookup' _ r t = r # (untuple t :: hl)

-- If label does not occur, add to front
class AddOrUpdateLabel l v r r' | l v r -> r' where
  addOrUpdateLabel :: l -> v -> r -> r'

instance (RecordLabels r ls
         ,OccursP l ls occ
         ,AddOrUpdateLabel' occ l v r r'
         ) => AddOrUpdateLabel l v r r' where
  addOrUpdateLabel = addOrUpdateLabel' (undefined::occ)
         
class AddOrUpdateLabel' occ l v r r' where
  addOrUpdateLabel' :: occ -> l -> v -> r -> r'
  
instance (UpdateLabel l v r r'
         ) => AddOrUpdateLabel' HTrue l v r r' where
  addOrUpdateLabel' _ = updateLabel
  
instance (AddLabel l v r r'
         ) => AddOrUpdateLabel' HFalse l v r r' where
  addOrUpdateLabel' _ = addLabel
  
class AddLabel l v r r' | l v r -> r' where
  addLabel :: l -> v -> r -> r'
  
instance (RecordLabels r ls
         ,HOccursNot l ls
         ,HExtend (LVPair l v) r r'
         ) => AddLabel l v r r' where
  addLabel l v r = (LVPair v) .*. r
  
class UpdateLabel l v r r' | l v r -> r' where
  updateLabel :: l -> v -> r -> r'
  
instance (UpdateLabel l v r r'
         ,HRLabelSet r'
         ) => UpdateLabel l v (Record r) (Record r') where
  updateLabel _ v (Record r) = mkRecord $ updateLabel (undefined::l) v r

instance (HEq l l' f
         ,UpdateLabel' f l v (HCons (LVPair l' v') r) r'
         ) => UpdateLabel l v (HCons (LVPair l' v') r) r' where
  updateLabel = updateLabel' (undefined::f)

class UpdateLabel' eq l v r r' | eq l v r -> r' where
  updateLabel' :: eq -> l -> v -> r -> r'

instance UpdateLabel' HTrue l v (HCons (LVPair l v') r) (HCons (LVPair l v) r) where
  updateLabel' _ _ v (HCons _ r) = HCons (LVPair v) r

instance (UpdateLabel l v r r'
         ) => UpdateLabel' HFalse l v (HCons p r) (HCons p r') where
  updateLabel' _ _ v (HCons p r) = HCons p $ updateLabel (undefined::l) v r

class AddField l v r r' | l v r -> r' where
  addField :: l -> v -> r -> r'
  
instance (AddField (Label l) v r r'
         ,HRLabelSet r'
         ) => AddField (Label l) v (Record r) (Record r') where
  addField l v (Record r) = mkRecord $ addField l v r
  
instance (AddNestedField l v r r'
         ,HRLabelSet r'
         ) => AddField (NestLabels l) v (Record r) (Record r') where
  addField (NestLabels ls) v (Record r) = mkRecord $ addNestedField ls v r

instance AddField (Label l) v HNil (HCons (LVPair (Label l) v) HNil) where
  addField l v _ = HCons (LVPair v) HNil
  
instance (HOccursNot (Label f) fs
         ,RecordLabels (HCons e l) fs
         ) => AddField (Label f) v (HCons e l) (HCons (LVPair (Label f) v) (HCons e l)) where
  addField l v r = HCons (LVPair v) r

class AddNestedField ls v l l' | ls v l -> l' where
  addNestedField :: ls -> v -> l -> l'

instance (AddField f v l l'
         ) => AddNestedField (HCons f HNil) v l l' where
  addNestedField (HCons f HNil) = addField f
  
instance (RecordLabels outer outerLs
         ,OccursP l outerLs occ
         ,AddNestedField' occ (HCons l ls) v outer outer' 
         ) => AddNestedField (HCons l ls) v outer outer' where
  addNestedField = addNestedField' (undefined::occ)

class AddNestedField' occ ls v outer outer' where
  addNestedField' :: occ -> ls -> v -> outer -> outer'
  
instance (AddNestedField fs v inner inner'
         ,AddOrUpdateLabel f inner' outer outer'
         ,HLookup (Record outer) f inner
         ,HRLabelSet outer
         ) => AddNestedField' HTrue (HCons f fs) v outer outer' where
  addNestedField' _ (HCons f fs) v outer 
    = addOrUpdateLabel f inner' outer
    where inner = hLookup (mkRecord outer) f
          inner' = addNestedField fs v inner
          
instance (AddOrUpdateLabel f inner outer outer'
         ,AddNestedField fs v EmptyRecord inner
         ) => AddNestedField' HFalse (HCons f fs) v outer outer' where
  addNestedField' _ (HCons f fs) v outer
    = addOrUpdateLabel f inner outer
    where inner = addNestedField fs v emptyRecord

class OccursP x xs occ | x xs -> occ
instance OccursP x HNil HFalse
instance (HEq x x' eq
         ,OccursP' eq x xs occ
         ) => OccursP x (HCons x' xs) occ

class OccursP' eq x xs occ | eq x xs -> occ
instance OccursP' HTrue x xs HTrue
instance (OccursP x xs occ) => OccursP' HFalse x xs occ 

{-
class MakeField f v r | f v -> r where
  makeField :: f -> v -> r 
  
instance MakeField (Label l) v (LVPair (Label l) v) where
  makeField _ v = LVPair v
  
instance (MakeField (NestLabels ls) v r
         ) => MakeField (NestLabels (HCons l ls)) v (LVPair l (Record (HCons r HNil))) where
  makeField (NestLabels (HCons l ls)) v = LVPair . mkRecord $ HCons (makeField (NestLabels ls) v) HNil
-}

class HProjectByFields fs r r' | fs r -> r' where
  hProjectByFields :: fs -> r -> r'
  
instance (HProjectByFields' fs src EmptyRecord out  
         ) => HProjectByFields fs src out where
  hProjectByFields fs src = hProjectByFields' fs src emptyRecord
  
class HProjectByFields' fs src dest dest' | fs src dest -> dest' where
  hProjectByFields' :: fs -> src -> dest -> dest'
  
instance HProjectByFields' HNil src dest dest  where
  hProjectByFields' _ _ dest = dest
  
instance (HProjectByFields' fs src dest dest'
         ,HLookup src f v
         ,AddField f v dest' dest''
         ) => HProjectByFields' (HCons f fs) src dest dest'' where
  hProjectByFields' (HCons f fs) src dest
    = addField f v dest'
    where dest' = hProjectByFields' fs src dest
          v = hLookup src f