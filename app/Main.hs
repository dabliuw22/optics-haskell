module Main where

import Lens.Persons
import Prism.Json

main :: IO ()
main = do
  mainLens
  mainPrism
