{-# LANGUAGE TemplateHaskell #-}
module Data.HList.Field.TH where

import Data.Char
import Language.Haskell.TH.Quote 
import Language.Haskell.TH.Syntax 
import Language.Haskell.TH
import qualified Data.HList.Field.Label as L
import Data.HList

mkLabels :: [String] -> Q [Dec]
mkLabels ls = liftM concat $ mapM mkLabel ls

mkLabel :: String -> Q [Dec]
mkLabel l = let lbl = label l
            in sequence [valD (varP $ mkName l) (normalB lbl) []]
  
label :: String -> ExpQ
label l = appE [| L.Label |] $ mkHListE l

mkHListE [] = [| HNil |]
mkHListE (c:cs) = 
  appE (appE [| HCons |] c') cs'
  where c' = charToExp c
        cs' = mkHListE cs

labelT l = appT [t| L.Label |] $ mkHListT l

mkHListT [] = [t| HNil |]
mkHListT (c:cs) = 
  appT (appT [t| HCons |] c') cs'
  where c' = charToType c
        cs' = mkHListT cs

hasFields :: String -> [(String, TypeQ)] -> Q [Dec]
hasFields cls fields
  = sequence [c, i]
  where c = classD preds clsName [t] [] []
        i = instanceD preds (appT (conT clsName) (varT $ mkName "t")) []
        preds = sequence $ map mkFieldPred fields
        t = PlainTV $ mkName "t"
        clsName = mkName cls
        
mkFieldPred :: (String, TypeQ) -> Q Pred        
mkFieldPred (name, tyQ)
  = classP hasField [labelT name, t, tyQ]
  where hasField = mkName "HasField"
        t = varT $ mkName "t"

charToType c | isLower c = appT [t| L.Lower |] (charToType $ toUpper c)

charToType 'A' = [t| L.A |]
charToType 'B' = [t| L.B |]
charToType 'C' = [t| L.C |]
charToType 'D' = [t| L.D |]
charToType 'E' = [t| L.E |]
charToType 'F' = [t| L.F |]
charToType 'G' = [t| L.G |]
charToType 'H' = [t| L.H |]
charToType 'I' = [t| L.I |]
charToType 'J' = [t| L.J |]
charToType 'K' = [t| L.K |]
charToType 'L' = [t| L.L |]
charToType 'M' = [t| L.M |]
charToType 'N' = [t| L.N |]
charToType 'O' = [t| L.O |]
charToType 'P' = [t| L.P |]
charToType 'Q' = [t| L.Q |]
charToType 'R' = [t| L.R |]
charToType 'S' = [t| L.S |]
charToType 'T' = [t| L.T |]
charToType 'U' = [t| L.U |]
charToType 'V' = [t| L.V |]
charToType 'W' = [t| L.W |]
charToType 'X' = [t| L.X |]
charToType 'Y' = [t| L.Y |]
charToType 'Z' = [t| L.Z |]

charToType '0' = [t| L.D0 |]
charToType '1' = [t| L.D1 |]
charToType '2' = [t| L.D2 |]
charToType '3' = [t| L.D3 |]
charToType '4' = [t| L.D4 |]
charToType '5' = [t| L.D5 |]
charToType '6' = [t| L.D6 |]
charToType '7' = [t| L.D7 |]
charToType '8' = [t| L.D8 |]
charToType '9' = [t| L.D9 |]

charToType '_' = [t| L.Underscore |]

charToExp c | isLower c = appE [| L.Lower |] (charToExp $ toUpper c)

charToExp 'A' = [| L.A |]
charToExp 'B' = [| L.B |]
charToExp 'C' = [| L.C |]
charToExp 'D' = [| L.D |]
charToExp 'E' = [| L.E |]
charToExp 'F' = [| L.F |]
charToExp 'G' = [| L.G |]
charToExp 'H' = [| L.H |]
charToExp 'I' = [| L.I |]
charToExp 'J' = [| L.J |]
charToExp 'K' = [| L.K |]
charToExp 'L' = [| L.L |]
charToExp 'M' = [| L.M |]
charToExp 'N' = [| L.N |]
charToExp 'O' = [| L.O |]
charToExp 'P' = [| L.P |]
charToExp 'Q' = [| L.Q |]
charToExp 'R' = [| L.R |]
charToExp 'S' = [| L.S |]
charToExp 'T' = [| L.T |]
charToExp 'U' = [| L.U |]
charToExp 'V' = [| L.V |]
charToExp 'W' = [| L.W |]
charToExp 'X' = [| L.X |]
charToExp 'Y' = [| L.Y |]
charToExp 'Z' = [| L.Z |]

charToExp '0' = [| L.D0 |]
charToExp '1' = [| L.D1 |]
charToExp '2' = [| L.D2 |]
charToExp '3' = [| L.D3 |]
charToExp '4' = [| L.D4 |]
charToExp '5' = [| L.D5 |]
charToExp '6' = [| L.D6 |]
charToExp '7' = [| L.D7 |]
charToExp '8' = [| L.D8 |]
charToExp '9' = [| L.D9 |]

charToExp '_' = [| L.Underscore |]