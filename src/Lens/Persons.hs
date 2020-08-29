{-# LANGUAGE TemplateHaskell #-}
module Lens.Persons (AddressStreet(..), CityName(..), Address (..),
                    PersonName(..), Person(..), mainLens) where

import Data.Char (toUpper)
import Control.Lens

newtype AddressStreet = AddressStreet { _s :: String } deriving Show
newtype CityName = CityName { _c :: String } deriving Show
data Address = Address {
  _street :: AddressStreet,
  _city :: CityName
} deriving Show
newtype PersonName = PersonName { _n :: String } deriving Show
data Person = Person {
  _name :: PersonName,
  _address :: Address
} deriving Show

person :: Person
person = Person {
  _name = PersonName { _n = "Nick Doe" },
  _address = Address {
    _street = AddressStreet { _s = "Wall Street" },
    _city = CityName { _c = "new York" }
  }
}

makeLenses ''AddressStreet
makeLenses ''CityName
makeLenses ''Address
makeLenses ''PersonName
makeLenses ''Person

newPerson =
  set (address . city . c) (capitalize $ view (address . city . c) person) person

newPersonTwo =
  (address . city . c) .~ capitalize (person ^. (address . city . c)) $ person

capitalize :: String -> String
capitalize [] = []
capitalize (h : t) = toUpper h : t

mainLens :: IO ()
mainLens = print newPersonTwo
