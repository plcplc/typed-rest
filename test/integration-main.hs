{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Network.HTTP.Types

import Network.HTTP.Rest.Client
import Network.HTTP.Rest.Signature
import Network.HTTP.Rest.Server

main :: IO ()
main = hspec $ clientServerCompatible

-- | Test that the typed-rest-client and typed-rest-server produce
-- compatible bindings.
clientServerCompatible :: Spec
clientServerCompatible = describe "The server and client" $
  it "are compatible" $ pending

type ConstantsSignature = RestSig (S "test" :/: S "constants" :/: Nil) ('HttpGet Bool)

constantServer :: PartialApplication
constantServer = serveRest (RestResource :: ConstantsSignature) (\() -> return (status200, [], True))

constantClient :: ...
