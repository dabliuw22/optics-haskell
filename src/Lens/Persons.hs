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

address' :: Lens' Person Address
address' = lens getter setter
  where
    getter :: Person -> Address
    getter = _address
    setter :: Person -> Address -> Person
    setter = \p a -> p { _address = a }
city' :: Lens' Address CityName
city' = lens _city $ \a c -> a { _city = c }
cityName' :: Lens' CityName String
cityName' = lens _c $ \c n -> c { _c = n }
compose' :: Lens' Person String
compose' = address' . city' . cityName'

makeLenses ''AddressStreet
makeLenses ''CityName
makeLenses ''Address
makeLenses ''PersonName
makeLenses ''Person

newPerson =
  set (address . city . c) (capitalize $ view (address . city . c) person) person

newPersonTwo =
  (address . city . c) .~ capitalize (person ^. (address . city . c)) $ person

newPersonThree =
  (address . city . c) %~ capitalize $ person
    
newPersonFour =
  over (address . city . c) capitalize person

newPersonFive =
  ((address . city . c) %~ capitalize) . ((name . n) .~ "Jhon Doe") $ person
  
newPersonSix =
  person & ((address . city . c) %~ capitalize) . ((name . n) .~ "Jhon Doe")
  
newPersonSeven =
  person 
    & ((address . city . c) %~ capitalize) 
    & ((name . n) .~ "Jhon Doe")

newPersonEight = compose' %~ capitalize $ person

capitalize :: String -> String
capitalize [] = []
capitalize (h : t) = toUpper h : t

mainLens :: IO ()
mainLens = print newPersonFive
