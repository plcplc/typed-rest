{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Network.HTTP.Rest.Server where

import Data.Maybe
import Data.Proxy
import Test.Hspec
import Network.HTTP.Types

import Network.HTTP.Rest.Signature
import Network.HTTP.Rest.Server

import Test.PayloadEncoding

type ConstantPath = S "foo" :/: S "bar" :/: Nil
constantPathPx     = Proxy :: Proxy ConstantPath

type ParametrisedPath = A "foo" Int :/: Nil
parametrisedPathPx = Proxy :: Proxy ParametrisedPath

nilPathPx          = Proxy :: Proxy Nil

type MethodGetBool = HttpGet Bool
methodGetBoolPx    = Proxy :: Proxy MethodGetBool

type MethodPostBool = HttpPost Bool Bool
methodPostBoolPx    = Proxy :: Proxy MethodPostBool

serverSpec :: Spec
serverSpec = describe "The typed-rest Server" $ do
  context "when pattern matching paths" $ do

    it "accepts nil correctly" $ do
      let constApp = servePath nilPathPx [] ()
      shouldSatisfy constApp isJust

    it "rejects nil correctly" $ do
      let constApp = servePath nilPathPx ["foo", "floo"] ()
      shouldBe constApp Nothing

    it "accepts constants correctly" $ do
      let constApp = servePath constantPathPx ["foo", "bar"] ()
      shouldSatisfy constApp isJust

    it "rejects constants correctly" $ do
      let constApp' = servePath constantPathPx ["foo", "floo"] ()
      shouldBe constApp' Nothing
      let constApp'' = servePath constantPathPx ["foo"] ()
      shouldBe constApp'' Nothing

    it "accepts parameters correctly" $ do
      let paramApp = servePath parametrisedPathPx ["7"] id
      shouldBe paramApp (Just 7)

    it "rejects parameters correctly" $ do
      let paramApp = servePath parametrisedPathPx ["foo"] (const ())
      shouldBe paramApp Nothing

  context "when pattern matching methods" $ do

    it "accepts GET requests correctly" $ do
      let getApp = serveMethod methodGetBoolPx identityEncPx GET Nothing True
      shouldSatisfy getApp isJust

    it "rejects GET requests correctly" $ do
      let getApp = serveMethod methodGetBoolPx identityEncPx POST Nothing True
      shouldSatisfy getApp (not . isJust)

    it "accepts POST requests correctly" $ do
      let postApp = serveMethod methodPostBoolPx identityEncPx POST (Just $ payloadEncode identityEncPx True) id
      shouldSatisfy postApp isJust

      let postApp' = serveMethod methodPostBoolPx identityEncPx POST (Just $ payloadEncode identityEncPx False) id
      shouldSatisfy postApp' isJust

    it "rejects POST requests correctly" $ do
      let postApp = serveMethod methodPostBoolPx identityEncPx GET Nothing id
      shouldSatisfy postApp (not .isJust)

  {-
  it "can properly pattern match PartialApplications" pending
  -}
