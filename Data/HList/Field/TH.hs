{-# LANGUAGE TemplateHaskell #-}
module Data.HList.Field.TH where

import Data.Char
import Language.Haskell.TH.Quote 
import Language.Haskell.TH.Syntax 
import Language.Haskell.TH
import qualified Data.HList.Field.Label
import Data.HList

labels :: [String] -> Q [Dec]
labels ls = liftM concat $ mapM label ls

label :: String -> Q [Dec]
label l = let lbl = labelE l
          in sequence [valD (varP $ mkName l) (normalB lbl) []]
  
labelE :: String -> ExpQ
labelE l = appE [| Data.HList.Field.Label.Label |] (strToExp l)

strToExp [] = [| HNil |]
strToExp (x:xs) = 
  infixE (Just x') [| HCons |] (Just xs')
  where x' = charToExp x
        xs' = strToExp xs

charToExp 'a' = [| Data.HList.Field.Label.a |]
charToExp 'b' = [| Data.HList.Field.Label.b |]
charToExp 'c' = [| Data.HList.Field.Label.c |]
charToExp 'd' = [| Data.HList.Field.Label.d |]
charToExp 'e' = [| Data.HList.Field.Label.e |]
charToExp 'f' = [| Data.HList.Field.Label.f |]
charToExp 'g' = [| Data.HList.Field.Label.g |]
charToExp 'h' = [| Data.HList.Field.Label.h |]
charToExp 'i' = [| Data.HList.Field.Label.i |]
charToExp 'j' = [| Data.HList.Field.Label.j |]
charToExp 'k' = [| Data.HList.Field.Label.k |]
charToExp 'l' = [| Data.HList.Field.Label.l |]
charToExp 'm' = [| Data.HList.Field.Label.m |]
charToExp 'n' = [| Data.HList.Field.Label.n |]
charToExp 'o' = [| Data.HList.Field.Label.o |]
charToExp 'p' = [| Data.HList.Field.Label.p |]
charToExp 'q' = [| Data.HList.Field.Label.q |]
charToExp 'r' = [| Data.HList.Field.Label.r |]
charToExp 's' = [| Data.HList.Field.Label.s |]
charToExp 't' = [| Data.HList.Field.Label.t |]
charToExp 'u' = [| Data.HList.Field.Label.u |]
charToExp 'v' = [| Data.HList.Field.Label.v |]
charToExp 'w' = [| Data.HList.Field.Label.w |]
charToExp 'x' = [| Data.HList.Field.Label.x |]
charToExp 'y' = [| Data.HList.Field.Label.y |]
charToExp 'z' = [| Data.HList.Field.Label.z |]

charToExp 'A' = [| Data.HList.Field.Label.A |]
charToExp 'B' = [| Data.HList.Field.Label.B |]
charToExp 'C' = [| Data.HList.Field.Label.C |]
charToExp 'D' = [| Data.HList.Field.Label.D |]
charToExp 'E' = [| Data.HList.Field.Label.E |]
charToExp 'F' = [| Data.HList.Field.Label.F |]
charToExp 'G' = [| Data.HList.Field.Label.G |]
charToExp 'H' = [| Data.HList.Field.Label.H |]
charToExp 'I' = [| Data.HList.Field.Label.I |]
charToExp 'J' = [| Data.HList.Field.Label.J |]
charToExp 'K' = [| Data.HList.Field.Label.K |]
charToExp 'L' = [| Data.HList.Field.Label.L |]
charToExp 'M' = [| Data.HList.Field.Label.M |]
charToExp 'N' = [| Data.HList.Field.Label.N |]
charToExp 'O' = [| Data.HList.Field.Label.O |]
charToExp 'P' = [| Data.HList.Field.Label.P |]
charToExp 'Q' = [| Data.HList.Field.Label.Q |]
charToExp 'R' = [| Data.HList.Field.Label.R |]
charToExp 'S' = [| Data.HList.Field.Label.S |]
charToExp 'T' = [| Data.HList.Field.Label.T |]
charToExp 'U' = [| Data.HList.Field.Label.U |]
charToExp 'V' = [| Data.HList.Field.Label.V |]
charToExp 'W' = [| Data.HList.Field.Label.W |]
charToExp 'X' = [| Data.HList.Field.Label.X |]
charToExp 'Y' = [| Data.HList.Field.Label.Y |]
charToExp 'Z' = [| Data.HList.Field.Label.Z |]

charToExp '0' = [| Data.HList.Field.Label.D0 |]
charToExp '1' = [| Data.HList.Field.Label.D1 |]
charToExp '2' = [| Data.HList.Field.Label.D2 |]
charToExp '3' = [| Data.HList.Field.Label.D3 |]
charToExp '4' = [| Data.HList.Field.Label.D4 |]
charToExp '5' = [| Data.HList.Field.Label.D5 |]
charToExp '6' = [| Data.HList.Field.Label.D6 |]
charToExp '7' = [| Data.HList.Field.Label.D7 |]
charToExp '8' = [| Data.HList.Field.Label.D8 |]
charToExp '9' = [| Data.HList.Field.Label.D9 |]

charToExp '_' = [| Data.HList.Field.Label.Underscore |]