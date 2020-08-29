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
getString = jsonString ^? stringPrism

makeLenses ''Json

jsonInt :: Json
jsonInt = JsonInt { _i = 1 }
newJsonInt = set i ((jsonInt ^. i) + 1) jsonInt

instance Semigroup Int where
  (<>) m1 m2 = m1 + m2

instance Monoid Int where
  mempty = 0
  
mainPrism :: IO ()
mainPrism = print newJsonInt
