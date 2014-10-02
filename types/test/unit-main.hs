module Main where

import Test.Hspec

import Test.Network.HTTP.Rest.Client
import Test.Network.HTTP.Rest.Server

main :: IO ()
main = hspec $ do
  clientSpec
  serverSpec
