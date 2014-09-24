{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- | This module defines the machinery for typed querying of REST API
-- signatures. It uses the functions and types from the @http-client@ package.
module Network.HTTP.Rest.Client (

  -- * Convenience wrappers
  requestHttpClient,
  requestHttpClientDef,
  buildUrl,

  -- * Typeclasses for requesting resources
  --RequestResource(..),
  RequestPath(..),
  RequestMethod(..),
  requestK,

  -- * Internal functions, exposed for testing mainly
  toPathComponents,
  fromPathComponents

  ) where

import Control.Applicative

import Data.Aeson (FromJSON,ToJSON, fromJSON, encode, Result(..))
import Data.Aeson.Parser (value')
import Data.Attoparsec.ByteString (maybeResult, parse)
import qualified Data.ByteString.UTF8 as BSU8
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Data.Text hiding (filter, foldr)

import GHC.TypeLits

import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types.Method (methodGet, methodPost)

import Network.HTTP.Rest.Signature

-- | Typeclass that enables construction of a url that matches a given HttpPath.
-- Note that @path :: HttpPathKind@.
class RequestPath (path :: HttpPathKind) where

  -- | An associated type that gives the function type that corresponds to the
  -- HttpPath. This means an argument of type @a@ for every @A \"symbol\" a@
  -- component.
  type RequestPathFn path result :: *

  -- | Guided by a HttpPath and an initial root path segment, we
  -- have a function that translates each of the variable components of @path@
  -- to function arguments.
  requestPath :: Proxy path -> ([String] -> result) -> [String] -> RequestPathFn path result

-- | The case for empty http paths.
instance RequestPath 'Nil where

  -- The case for 'Nil is just 'result' - no more path segments to construct.
  type RequestPathFn 'Nil result = result

  requestPath :: Proxy 'Nil -> ([String] -> result) -> [String] -> RequestPathFn 'Nil result
  requestPath _ k p = k p

-- | The case for literal http path components.
instance (
    RequestPath rest,
    KnownSymbol path
    ) => RequestPath (S path :/: rest) where

  -- The case for @S sig :/: rest@ reduces to @rest@.
  type RequestPathFn (S path :/: rest) result = RequestPathFn rest result

  requestPath ::
       Proxy (S path :/: rest)
    -> ([String] -> result)
    -> [String]
    -> RequestPathFn (S path :/: rest) result

  requestPath _ k p = requestPath (Proxy :: Proxy rest) k (p ++ [symbolVal (Proxy :: Proxy path)])

-- | The RequestPath case for variable http path components.
instance (
  HttpPathArgument a,
  RequestPath rest)
  => RequestPath (A path a :/: rest) where

  -- The case for 'A sig a :/: rest' gives an actual function.
  type RequestPathFn (A path a :/: rest) result = a -> RequestPathFn rest result

  requestPath ::
    Proxy (A path a :/: rest)
    -> ([String] -> result)
    -> [String]
    -> RequestPathFn (A path a :/: rest) result
  requestPath _ k p x = requestPath (Proxy :: Proxy rest) k
    (p ++ [unpack $ toPathArg x])

-- | Convenience wrapper around 'requestPath' for urls without a prefix.
-- (initial "/" is added automatically)
buildUrl :: RequestPath path => Proxy path -> RequestPathFn path String
buildUrl p = requestPath p fromPathComponents []

-- | Type class to construct a function that constructs Requests based on
-- a 'method :: HttpMethodKind'.
class RequestMethod (method :: HttpMethodKind) where

  type RequestMethodFn (f :: * -> *) method :: *

  requestMethod :: Functor f =>
       Proxy method
    -> (Request -> f LBS.ByteString)
    -> Request
    -> RequestMethodFn f method

-- | The RequestMethod instance for GET requests.
instance (FromJSON resp) => RequestMethod ('HttpGet resp) where

  type RequestMethodFn f ('HttpGet resp) = f resp

  requestMethod :: Functor f =>
       Proxy ('HttpGet resp)
    -> (Request -> f LBS.ByteString)
    -> Request
    -> RequestMethodFn f ('HttpGet resp)

  requestMethod _ act request = parseNonRfcJson <$> act (setMethod request)

    where

      setMethod :: Request -> Request
      setMethod req = req { method = methodGet }

-- | RFC-4627 sucks cocks in hell. We have to go all the way around
-- Data.Aeson and use the low-level attoparsec tools to get beyond the
-- only-objects-at-top-level restriction. Why on earth would you sacrifice
-- having encode/decode an isomorphism just to comply with an RFC?.
-- Sorry potential readers, I'm just venting steam...
parseNonRfcJson :: FromJSON a => LBS.ByteString -> a
parseNonRfcJson lbs = let
  bs          = LBS.toStrict lbs
  Just val    = maybeResult $ parse value' bs
  Success res = fromJSON val
  in res

-- | The RequestResource instance for POST requests.
instance
  (ToJSON body,
  FromJSON resp)
  => RequestMethod ('HttpPost body resp) where

  type RequestMethodFn f ('HttpPost body resp) = body -> f resp

  requestMethod :: Functor f =>
       Proxy ('HttpPost body resp)
    -> (Request -> f LBS.ByteString)
    -> Request
    -> RequestMethodFn f ('HttpPost body resp)

  requestMethod _ act request body = parseNonRfcJson <$> act (setMethod request)

    where

      setMethod :: Request -> Request
      setMethod req = req {
          method = methodPost,
          requestBody = RequestBodyLBS (encode body)
        }

-- | Given an continuation capable of executing requests, construct and
-- execute requests using 'RequestPathFn' and 'RequestMethodFn'.
requestK ::
  (Functor f, RequestPath path, RequestMethod method) =>
     Proxy (RestSig path method)
  -> (Request -> f LBS.ByteString)
  -> Request
  -> RequestPathFn path (RequestMethodFn f method)
requestK (_ :: Proxy (RestSig path method)) act request = requestPath
    pathProxy (requestMethod methodProxy act . setPath) basePath

  where
    pathProxy   = Proxy :: Proxy path
    methodProxy = Proxy :: Proxy method
    basePath  = toPathComponents $ BSU8.toString $ path request
    setPath p = request { path = BSU8.fromString $ fromPathComponents p }

toPathComponents :: String -> [String]
toPathComponents = filter (/=[]) . foldr splitPath []
  where
    splitPath '/' ps      = []:ps
    splitPath c   (p:ps)  = (c:p):ps
    splitPath c   []      = [[c]]

fromPathComponents :: [String] -> String
fromPathComponents [] = "/"
fromPathComponents (p:ps) = '/': (p ++ fromPathComponents ps)

-- | Actually execute a request constructed with 'RequestPathFn' and
-- 'RequestMethodFn'.
requestHttpClient ::
  (RequestPath path, RequestMethod method) =>
    Proxy (RestSig path method) ->
    Manager ->
    Request ->
    RequestPathFn path (RequestMethodFn IO method)
requestHttpClient p man baseReq = requestK p ((responseBody <$>) . flip httpLbs man) baseReq

-- | Actually execute a request constructed with 'RequestPathFn' and
-- 'RequestMethodFn'. If you worry about resource management, use
-- 'requestHttpClient' instead.
requestHttpClientDef ::
  (RequestPath path, RequestMethod method) =>
    Proxy (RestSig path method) ->
    Request ->
    RequestPathFn path (RequestMethodFn IO method)

requestHttpClientDef p baseReq = requestK p
  (\req -> do
    man <- newManager defaultManagerSettings
    resp <- responseBody <$> httpLbs req man
    closeManager man
    return resp) baseReq
