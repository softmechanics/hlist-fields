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
{-
(=:) :: (AddOrUpdateLabel l v cols cols'
        ,HRLabelSet cols'
        ) => l -> v -> Record cols -> Record cols'
(=:) l v obj = addOrUpdateLabel l v obj

(=~) :: (HUpdateAtHNat n (LVPair l v') cols cols'
        ,HFind l ls n
        ,RecordLabels cols ls
        ,HasField l (Record cols) v
        ,HLookup (Record cols) l v
        ) => l -> (v -> v') -> Record cols -> Record cols'
(=~) l f obj = hUpdateAtLabel l (f $ obj # l) obj
-}

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
         ,HRLabelSet cols'
         ) => HApply (EqualColon l v) (Record cols) (Record cols') where
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
         ,pR ~ fR
         ,fR' ~ fsR
         ,pR' ~ fsR'
         ) =>  HApplyAll (HCons f fs) pR pR' where
  hApplyAll (HCons f fs) r = hApplyAll fs $ hApply f r

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
  
instance (H2ProjectByLabels (HCons l ls) t t' a
         ,HRLabelSet t'
         ) => HLookup' HFalse (Record t) (HCons l ls) (Record t') where
  hLookup' _ r ls = hProjectByLabels ls r

instance (Untuple t hl
         ,HLookup (Record r) hl r'
         ) => HLookup' HTrue (Record r) t r' where
  hLookup' _ r t = r # (untuple t :: hl)



class AddOrUpdateLabel l v r r' | l v r -> r' where
  addOrUpdateLabel :: l -> v -> r -> r'

instance (AddOrUpdateLabel l v r r'
         ,HRLabelSet r'
         ) => AddOrUpdateLabel l v (Record r) (Record r') where
  addOrUpdateLabel _ v (Record r) = mkRecord $ addOrUpdateLabel (undefined::l) v r

instance AddOrUpdateLabel l v HNil (HCons (LVPair l v) HNil) where
  addOrUpdateLabel _ v HNil = (LVPair v) .*. HNil

instance (HEq l l' f
         ,AddOrUpdateLabel' f l v (HCons (LVPair l' v') r) r'
         ) => AddOrUpdateLabel l v (HCons (LVPair l' v') r) r' where
  addOrUpdateLabel = addOrUpdateLabel' (undefined::f)

class AddOrUpdateLabel' eq l v r r' | eq l v r -> r' where
  addOrUpdateLabel' :: eq -> l -> v -> r -> r'

instance AddOrUpdateLabel' HTrue l v (HCons (LVPair l' v') r) (HCons (LVPair l v) r) where
  addOrUpdateLabel' _ _ v (HCons _ r) = HCons (LVPair v) r

instance (AddOrUpdateLabel l v r r') => AddOrUpdateLabel' HFalse l v (HCons p r) (HCons p r') where
  addOrUpdateLabel' _ _ v (HCons p r) = HCons p $ addOrUpdateLabel (undefined::l) v r

