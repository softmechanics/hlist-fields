{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , FunctionalDependencies 
           , TypeFamilies
           , OverlappingInstances
           , UndecidableInstances #-}

module Data.HList.Field.Base (
  module Data.HList,
  Label(..),
  HLookup(..), 
  DotHash(..),
  (=:), (=~),

  TypeEq(..)
  ) where

import Data.HList.Record
import Data.HList.Tuple
import Data.HList hiding (TypeEq, (#))
import Data.HList.TypeEqGeneric2
import Data.HList.Label1 (Label(..))

instance (TypeEq a b f, HBool f) => HEq a b f

infixr 9 =:, =~
{-
(=:) :: (HUpdateAtHNat n (LVPair l v) cols cols'
        ,HFind l ls n
        ,RecordLabels cols ls
        ) => l -> v -> Record cols -> Record cols'
(=:) l v obj = hUpdateAtLabel l v obj
-}

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

--newtype Label a = Label a deriving Show

infixr 8 .#
class DotHash a b c | a b -> c where
  (.#) :: a -> b -> c

instance a ~ b => DotHash (Record a) (Record b -> Record c) (Record c) where
  rec .# f = f rec

instance (HUpdateAtHNat n (LVPair (Label l) (Record inner')) out out'
         ,HFind (Label l) ls n
         ,RecordLabels out ls
         ,HasField (Label l) (Record out) (Record inner)
         ) => DotHash (Label l) (Record inner -> Record inner') (Record out -> Record out') where
  l .# f = \out -> let inner = out # l 
                   in hUpdateAtLabel l (f inner) out

class HLookup a b c | a b -> c where
  (#) :: a -> b -> c

instance (IsTuple l f
         ,HLookup' f r l r'
         ) => HLookup r l r' where
  (#) = hLookup' (undefined::f)
  
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

