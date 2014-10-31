{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
module Test.Network.HTTP.Rest.Server where

import Control.Monad.Identity
import Data.Proxy
import Test.Hspec
import Network.HTTP.Types

import Network.HTTP.Rest.Match
import Network.HTTP.Rest.Server
import Network.HTTP.Rest.Signature

import Test.Network.HTTP.Rest.Match
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
      let constApp = servePath nilPathPx [] (return ())
      shouldSatisfy constApp isMatchSuccess

    it "rejects nil correctly" $ do
      let constApp = servePath nilPathPx ["foo", "floo"] (return ())
      shouldSatisfy constApp isMatchFailed

    it "accepts constants correctly" $ do
      let constApp = servePath constantPathPx ["foo", "bar"] (return ())
      shouldSatisfy constApp isMatchSuccess

    it "rejects constants correctly" $ do
      let constApp' = servePath constantPathPx ["foo", "floo"] (return ())
      shouldSatisfy constApp' isMatchFailed
      let constApp'' = servePath constantPathPx ["foo"] (return ())
      shouldSatisfy constApp'' isMatchFailed

    it "accepts parameters correctly" $ do
      let paramApp = servePath parametrisedPathPx ["7"] (return id)
      shouldSatisfy paramApp (matched 7)

    it "rejects parameters correctly" $ do
      let paramApp = servePath parametrisedPathPx ["foo"] (return $ const ())
      shouldSatisfy paramApp isMatchFailed

  context "when pattern matching methods" $ do

    it "accepts GET requests correctly" $ do
      let getApp = serveMethod methodGetBoolPx identityEncPx GET Nothing (return $ Identity True)
      shouldSatisfy getApp isMatchSuccess

    it "rejects GET requests correctly" $ do
      let getApp = serveMethod methodGetBoolPx identityEncPx POST Nothing (return $ Identity True)
      shouldSatisfy getApp isMatchFailed

    it "accepts POST requests correctly" $ do
      let postApp = serveMethod methodPostBoolPx identityEncPx POST
            (Just $ payloadEncode identityEncPx True) (return Identity)
      shouldSatisfy postApp isMatchSuccess

      {-
      let postApp' = serveMethod methodPostBoolPx identityEncPx POST
          (Just $ payloadEncode identityEncPx False) (return Identity)
      shouldSatisfy postApp' (matched $ Identity False)
      -}

    it "rejects POST requests correctly" $ do
      let postApp = serveMethod methodPostBoolPx identityEncPx GET
            Nothing (return Identity)
      shouldSatisfy postApp isMatchFailed

  {-
  it "can properly pattern match PartialApplications" pending
  -}

-- For 'shouldSatisfy'.
instance (Show a) => Show (Identity a) where
  show (Identity x) = "Identity " ++ show x
