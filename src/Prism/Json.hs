{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Prism.Json (Json(..), mainPrism) where

import Control.Lens

data Json = JsonNull
  | JsonString { _s :: String }
  | JsonInt { _i :: Int} deriving Show

stringPrism :: Prism' Json String
stringPrism =
  prism' JsonString
    (\case
        JsonString v -> Just v
        _            -> Nothing
    )
jsonString :: Json
jsonString = JsonString { _s = "string" }
getString = preview stringPrism jsonString
getStringTwo = jsonString ^? stringPrism
newJsonString = review stringPrism "String"
newJsonStringTwo = stringPrism # "String"

makeLenses ''Json

jsonInt :: Json
jsonInt = JsonInt { _i = 1 }
newJsonInt :: Json
newJsonInt = set i ((jsonInt ^. i) + 1) jsonInt
getInt :: Maybe Int
getInt = jsonInt ^? i

instance Semigroup Int where
  (<>) a b = a + b

instance Monoid Int where
  mempty = 0
  
mainPrism :: IO ()
mainPrism = print newJsonInt
