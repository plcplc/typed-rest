{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | This module wires the http-client package to query Rest resources
-- defined by RestSigs.
module Network.HTTP.Rest.Client.HttpClient
  ( requestHttpClient, requestHttpClientDef ) where

import Control.Applicative

import qualified Data.ByteString.UTF8 as BSU8
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy

import Network.HTTP.Client as HC hiding (Proxy)
import Network.HTTP.Types

import Network.HTTP.Rest.Client as RC
import Network.HTTP.Rest.Signature

-- | Actually execute a request constructed with 'RequestPathFn' and
-- 'RequestMethodFn'. To allow for network resource management, requests
-- are made in the context of a 'Manager'.
requestHttpClient ::
  (RC.Request path method encoding, EncodedRepr encoding ~ LBS.ByteString) =>
    Manager ->
    HC.Request ->
    Proxy (RestSig path method encoding) ->
    RequestPathFn path (RequestMethodFn IO method)
requestHttpClient man baseReq (sigPx :: Proxy (RestSig path method encoding)) =
  requestResource sigPx (performReq man baseReq)

-- | Internal
performReq ::
  Manager
  -> HC.Request
  -> [String]
  -> StdMethod
  -> Maybe LBS.ByteString
  -> IO LBS.ByteString
performReq man baseReq p m maybeBody = do
  let finalReq = baseReq {
                      method = renderMethod (Right m),
                      path   = BSU8.fromString $ fromPathComponents p,
                      requestBody = RequestBodyLBS $ maybe LBS.empty id maybeBody
                      }
  responseBody <$> httpLbs finalReq man

-- | Actually execute a request constructed with 'RequestPathFn' and
-- 'RequestMethodFn'. This version naively creates a fresh 'Manager' on
-- each request.
requestHttpClientDef ::
  (RC.Request path method encoding, EncodedRepr encoding ~ LBS.ByteString) =>
    HC.Request ->
    Proxy (RestSig path method encoding) ->
    RequestPathFn path (RequestMethodFn IO method)
requestHttpClientDef baseReq (sigPx :: Proxy (RestSig path method encoding)) =
  requestResource sigPx
    (\p m maybeBody -> do
      man <- newManager defaultManagerSettings
      resp <- performReq man baseReq p m maybeBody
      closeManager man
      return resp
      )

