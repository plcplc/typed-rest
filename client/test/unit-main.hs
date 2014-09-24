module Main where

import Test.Hspec

import Test.Network.HTTP.Rest.Client

main :: IO ()
main = hspec $
  clientSpec
