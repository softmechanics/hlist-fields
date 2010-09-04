{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Data.HList.Field.Label where
import Data.HList
import Data.Char
       
-- | Types for constructing labels without defining new types.  
-- Ensures that a label myLabel defined in one module is the same type
-- as another myLabel defined in another.
-- 
-- import qualified Data.HList.Label as L
-- myLabel = Label (L.m .*. L.y .*. L.L .*. L.a .*. L.b .*. L.e .*. L.l .*. HNil)
-- 
-- [$label| myLabel |]

newtype Label a = Label a

instance (ToStr l) => Show (Label l) where
  show _ = toStr (undefined::l)

instance (ToStr l) => ShowLabel (Label l) where
  showLabel _ = toStr (undefined::l)

newtype Lower a = Lower a

data A = A
data B = B
data C = C
data D = D
data E = E
data F = F
data G = G
data H = H
data I = I
data J = J
data K = K
data L = L
data M = M
data N = N
data O = O
data P = P
data Q = Q
data R = R
data S = S
data T = T
data U = U
data V = V
data W = W
data X = X
data Y = Y
data Z = Z
         
a = Lower A
b = Lower B
c = Lower C
d = Lower D
e = Lower E
f = Lower F
g = Lower G
h = Lower H
i = Lower I
j = Lower J
k = Lower K
l = Lower L
m = Lower M
n = Lower N
o = Lower O
p = Lower P
q = Lower Q
r = Lower R
s = Lower S
t = Lower T
u = Lower U
v = Lower V
w = Lower W
x = Lower X
y = Lower Y
z = Lower Z

data D0 = D0
data D1 = D1
data D2 = D2
data D3 = D3
data D4 = D4
data D5 = D5
data D6 = D6
data D7 = D7
data D8 = D8
data D9 = D9

data Underscore = Underscore

class ToStr a where
  toStr :: a -> [Char]
  
instance ToStr HNil where
  toStr _ = []
  
instance (ToChar a, ToStr b) => ToStr (HCons a b) where
  toStr _ = toChar (undefined::a) : toStr (undefined::b)

class ToChar a where
  toChar :: a -> Char
  
instance (ToChar c) => ToChar (Lower c) where
  toChar _ = toLower $ toChar (undefined::c)
  
instance ToChar A where
  toChar _ = 'A'
instance ToChar B where
  toChar _ = 'B'
instance ToChar C where
  toChar _ = 'C'
instance ToChar D where
  toChar _ = 'D'
instance ToChar E where
  toChar _ = 'E'
instance ToChar F where
  toChar _ = 'F'
instance ToChar G where
  toChar _ = 'G'
instance ToChar H where
  toChar _ = 'H'
instance ToChar I where
  toChar _ = 'I'
instance ToChar J where
  toChar _ = 'J'
instance ToChar K where
  toChar _ = 'K'
instance ToChar L where
  toChar _ = 'L'
instance ToChar M where
  toChar _ = 'M'
instance ToChar N where
  toChar _ = 'N'
instance ToChar O where
  toChar _ = 'O'
instance ToChar P where
  toChar _ = 'P'
instance ToChar Q where
  toChar _ = 'Q'
instance ToChar R where
  toChar _ = 'R'
instance ToChar S where
  toChar _ = 'S'
instance ToChar T where
  toChar _ = 'T'
instance ToChar U where
  toChar _ = 'U'
instance ToChar V where
  toChar _ = 'V'
instance ToChar W where
  toChar _ = 'W'
instance ToChar X where
  toChar _ = 'X'
instance ToChar Y where
  toChar _ = 'Y'
instance ToChar Z where
  toChar _ = 'Z'

instance ToChar D1 where
  toChar _ = '1'
instance ToChar D2 where
  toChar _ = '2'
instance ToChar D3 where
  toChar _ = '3'
instance ToChar D4 where
  toChar _ = '4'
instance ToChar D5 where
  toChar _ = '5'
instance ToChar D6 where
  toChar _ = '6'
instance ToChar D7 where
  toChar _ = '7'
instance ToChar D8 where
  toChar _ = '8'
instance ToChar D9 where
  toChar _ = '9'
instance ToChar D0 where
  toChar _ = '0'

instance ToChar Underscore where
  toChar _ = '_'
 