{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Network.HTTP.Rest.Client where

import Test.Hspec

import Data.Maybe
import Data.Proxy
import Network.HTTP.Types
import Network.HTTP.Rest.Client
import Network.HTTP.Rest.Signature

import Test.PayloadEncoding

type ConstantPath = (S "signature" :/: S "with" :/: S "constants" :/: S "only" :/: Nil)
type VariablePath = (S "signature" :/: S "with" :/: A "variables" Int :/: Nil)

type MethodGetString = HttpGet String
type MethodPostString = HttpPost String String

type ConstantGetSig = RestSig ConstantPath MethodGetString IdentityEncoding
type ConstantPostSig = RestSig ConstantPath MethodPostString IdentityEncoding
type NilSig = RestSig Nil MethodGetString

nilPathPx   = Proxy :: Proxy Nil
nilSigPx    = Proxy :: Proxy NilSig
constPathPx = Proxy :: Proxy ConstantPath
variablePathPx = Proxy :: Proxy VariablePath
constGetSigPx  = Proxy :: Proxy ConstantGetSig
constPostSigPx  = Proxy :: Proxy ConstantPostSig

clientSpec :: Spec
clientSpec = describe "The typed-rest client" $ do

  it "composes constant paths correctly" $ do
    let path = requestPath constPathPx id []
    shouldBe path ["signature", "with", "constants", "only"]

  it "composes variable paths correctly" $ do
    let path = requestPath variablePathPx id [] 42
    shouldBe path ["signature", "with", "42"]

  it "dispatches GET requests correctly" $ do
    res <- requestResource constGetSigPx (\path method body -> do
      shouldBe path ["signature", "with", "constants", "only"]
      shouldBe method GET
      shouldSatisfy body (not . isJust)
      return $ payloadEncode identityEncPx ("Test" :: String))

    shouldBe res (Success "Test")

  it "dispatches POST requests correctly" $ do
    res <- requestResource constPostSigPx (\path method body -> do
      shouldBe path ["signature", "with", "constants", "only"]
      shouldBe method POST
      shouldSatisfy body isJust
      shouldBe (payloadDecode identityEncPx =<< body) (Just ("Foo" :: String))
      return $ payloadEncode identityEncPx ("Test" :: String)) "Foo"

    shouldBe res (Success ("Test" :: String))
