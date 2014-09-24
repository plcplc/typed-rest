{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Network.HTTP.Rest.Client where

import Test.Hspec

import Control.Monad.Identity
import Data.Aeson
import Data.ByteString()
import Data.Proxy
import Data.String
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Rest.Client
import Network.HTTP.Rest.Signature

type ConstantPath = (S "signature" :/: S "with" :/: S "constants" :/: S "only" :/: Nil)

type MethodGetString = HttpGet String

type ConstantSig = RestSig ConstantPath MethodGetString
type NilSig = RestSig Nil MethodGetString

nilPathProxy   = Proxy :: Proxy Nil
nilSigProxy    = Proxy :: Proxy NilSig
constPathProxy = Proxy :: Proxy ConstantPath
constSigProxy  = Proxy :: Proxy ConstantSig

clientSpec :: Spec
clientSpec = describe "The typed-rest client" $ do

  it "produces well-formed request paths" $ do
    let actual = buildUrl constPathProxy
    shouldBe actual "/signature/with/constants/only/"

  it "splits paths correctly" $ do
    shouldBe (toPathComponents "/foo/bar/") ["foo","bar"]
    shouldBe (toPathComponents "foo/bar/") ["foo","bar"]
    shouldBe (toPathComponents "/foo/bar") ["foo","bar"]
    shouldBe (toPathComponents "foo/bar") ["foo","bar"]

  it "combines paths correctly" $ do
    shouldBe (fromPathComponents ["foo","bar"]) "/foo/bar/"
    shouldBe (fromPathComponents []) "/"

  it "appends paths correctly" $ do
    testRequest constSigProxy "https://example.com/foo" "/foo/signature/with/constants/only/"
    testRequest constSigProxy "https://example.com/foo/" "/foo/signature/with/constants/only/"
    testRequest nilSigProxy "https://example.com/foo/" "/foo/"
    testRequest nilSigProxy "https://example.com/foo" "/foo/"

  where

    -- | Perform a request that just json-encodes the result of 'show'.
    requestShow p = requestK p (Identity . encode . show)

    testRequest ::
      (RequestPath path, RequestMethod method,
       RequestPathFn path (RequestMethodFn Identity method) ~ Identity String)
      => Proxy (RestSig path method)
      -> String
      -> String
      -> Expectation
    testRequest sig urlStr expectPath = do
      url <- parseUrl urlStr :: IO Request
      let Identity r = requestShow sig url
      -- Requests don't have equality :-( so we compare the result of
      -- 'show' instead.
      shouldBe r (show url{ path = fromString expectPath})
