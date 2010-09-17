{-# LANGUAGE TemplateHaskell
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , MultiParamTypeClasses 
           #-}

import Data.HList.Field
import Data.HList.Field.TH
import Data.HList (HNil, HasField(..), emptyRecord)

$(mkLabel "name")
$(mkLabel "age")
$(mkLabel "cell")

kw = emptyRecord

-- allow optional fields, but only if they are Maybe types.
instance HasField l HNil (Maybe a) where
  hLookupByLabel _ _ = Nothing
  
$(hasFields "AddFriendKw" [("name",   [t| String    |])
                          ,("age",    [t| Int       |])
                          ,("cell",   [t| Maybe Int |])
                          ])

{-
The above Template Haskell macro produces:

class (HasField $(labelT "name") t String
      ,HasField $(labelT "age") t Int
      ,HasField $(labelT "cell") t (Maybe Int)
      ) => AddFriendKw t
      
instance (HasField $(labelT "name") t String
         ,HasField $(labelT "age") t Int
         ,HasField $(labelT "cell") t (Maybe Int)
         ) => AddFriendKw t
-}

addFriend :: (AddFriendKw kw) => kw -> IO ()
addFriend kw =
  let (n, a, c) = kw <# (name, age, cell)
  in print (n,a,c)

        
main = do addFriend $ kw # (name =: "Fred", 
                            age  =: 29)
          
          addFriend $ kw # (name =: "Bob", 
                            age  =: 27, 
                            cell =: Just 1234567890)
     