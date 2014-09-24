{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
-- | This module defines the machinery for serving typed REST API signatures,
-- by utilising the Wai server infrastructure. It is capable of wrapping
-- resource serving functions (with type signatures corresponding to given
-- 'RestSig's) as Wai middleware.
module Network.HTTP.Rest.Server (

  -- * Serving Resource Signatures
  ServeResource(..),
  ApplyHttpPathFn(..),
  ResponseType,
  PartialApplication(..),
  runPartialApplication,
  toPartialApplication

  ) where

import GHC.TypeLits

import Control.Monad


import Data.Aeson (FromJSON,ToJSON, decode, encode)
import Data.Monoid
import Data.Text (Text, pack)
import Data.Typeable

import Network.HTTP.Types
-- import Network.HTTP.Types.Header (ResponseHeaders)
-- import Network.HTTP.Types.Status (Status, status404, status500)
import Network.Wai (Application, Request(..), Response, ResponseReceived, lazyRequestBody, responseLBS)

import Network.HTTP.Rest.Signature

-- | A response type that manually encodes aspects of a http response.
type ResponseType a = (Status, ResponseHeaders, a)

-- | A type for WAI-Applications that pattern match on the request before
-- executing. The Monoid instance executes the first matching
-- PartialApplication. This can be used to model the serving of resources
-- identified by url.
newtype PartialApplication =
  PA { unPA :: Request -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived) }

instance Monoid PartialApplication where
  mempty = PA $ const Nothing
  mappend (PA pa1) (PA pa2) = PA $
    \req -> case pa1 req of
      Nothing -> pa2 req
      res     -> res

-- | Run a 'PartialApplication'. It returns 404 if the 'PartialApplication'
-- doesn't match.
runPartialApplication :: PartialApplication -> Application
runPartialApplication (PA pa) req respond =
  case pa req of
    Just res -> res respond
    Nothing -> respond $ responseLBS status404 [] "No resources match the request"

toPartialApplication :: Application -> PartialApplication
toPartialApplication app = PA $ \req -> Just $ app req

-- | A typeclass to apply resource-serving functions, 'HttpPathFn', to slightly
-- decomposed requests (consisting of a list of path components and a
-- deserialised request payload), giving 'Nothing' if the request path did not
-- match the signature. Note that 'path :: HttpPathKind'.
class ApplyHttpPathFn (path :: HttpPathKind) where

  -- | A type family for translating HttpPathKind types with response types to
  -- function types.
  type HttpPathFn path req resp :: *

  -- | A function to apply 'HttpPathFn's to the path they match.
  applyHttpPathFn :: Proxy path -> [Text] -> HttpPathFn path req resp -> Maybe (req -> IO (ResponseType resp))

-- | The 'ApplyHttpPathFn' case for empty paths.
instance ApplyHttpPathFn Nil where

  type HttpPathFn Nil req resp = req -> IO (ResponseType resp)

  applyHttpPathFn :: Proxy Nil -> [Text] -> HttpPathFn Nil req resp -> Maybe (req -> IO (ResponseType resp))
  applyHttpPathFn _ [] fn = Just fn
  applyHttpPathFn _ _  _  = Nothing


-- | The 'ApplyHttpPathFn' case for variable path components (that pass arguments to the given 'HttpPathFn').
instance
  (ApplyHttpPathFn rest,
  KnownSymbol pathSig,
  Typeable a,
  HttpPathArgument a) =>
    ApplyHttpPathFn (A pathSig a :/: rest) where

  type HttpPathFn (A pathSig a :/: rest) req resp = a -> HttpPathFn rest req resp

  applyHttpPathFn ::
    Proxy (A pathSig a :/: rest) -> [Text] ->
    HttpPathFn (A pathSig a :/: rest) req resp -> Maybe (req -> IO (ResponseType resp))

  applyHttpPathFn (Proxy :: Proxy (A pathSig a :/: rest)) (path:pathRest) fn = let
    pathSig' = symbolVal (Proxy :: Proxy pathSig)
    argMaybe = fromPathArg path
    in case (path == pack pathSig', argMaybe) of
      (True, Just arg) -> applyHttpPathFn (Proxy :: Proxy rest) pathRest (fn arg)
      _                -> Nothing
  applyHttpPathFn _ [] _ = Nothing

-- | The 'ApplyHttpPathFn' case for constant literal path components (that do not pass arguments to the given 'HttpPathFn').
instance
  (ApplyHttpPathFn rest, KnownSymbol pathSig) =>
  ApplyHttpPathFn (S pathSig :/: rest) where

  type HttpPathFn (S pathSig :/: rest) req resp = HttpPathFn rest req resp

  applyHttpPathFn ::
    Proxy (S pathSig :/: rest) -> [Text] ->
    HttpPathFn (S pathSig :/: rest) req resp -> Maybe (req -> IO (ResponseType resp))

  applyHttpPathFn (Proxy :: Proxy (S pathSig :/: rest)) (path:pathRest) fn = let
    pathSig' = symbolVal (Proxy :: Proxy pathSig)
    in case path == pack pathSig' of
      True -> applyHttpPathFn (Proxy :: Proxy rest) pathRest fn
      _    -> Nothing
  applyHttpPathFn _ [] _ = Nothing


-- | A typeclass to wrap 'HttpPathFn's into 'Application's.  Note that @method
-- :: HttpMethodKind@ and @path :: HttpPathKind@.  This class is the primary
-- interface to this module, and 'serveRest' should satisfy all your needs.
class (ApplyHttpPathFn pathSig) =>
  ServeResource (pathSig :: HttpPathKind) (method :: HttpMethodKind) where

  -- | An associated type that gives the final resulting function signature to
  -- serve a resource.
  type HttpMethodFn pathSig method :: *

  -- | A function that wraps a 'HttpPathFn' in a piece of Wai 'Middleware'. For requests
  -- that don't match, we delegate to another 'Application'.
  serveRest ::
    RestSig pathSig method -> HttpMethodFn pathSig method -> PartialApplication

-- | The instance for serving GET requests.
instance (ApplyHttpPathFn pathSig, ToJSON resp) => ServeResource pathSig ('HttpGet resp) where

  type HttpMethodFn pathSig ('HttpGet resp) = HttpPathFn pathSig () resp

  serveRest ::
    RestSig pathSig ('HttpGet resp) -> HttpMethodFn pathSig ('HttpGet resp) -> PartialApplication
  serveRest _ fn = PA $ \request -> do
    let path = pathInfo request
    when (requestMethod request /= methodGet) Nothing
    act <- applyHttpPathFn (Proxy :: Proxy pathSig) path fn :: Maybe (() -> IO (ResponseType resp))
    return $ \respondC -> do
      (status, headers, res) <- act ()
      respondC $ responseLBS status headers (encode res)

-- | The instance for serving POST requests. If we're unable to decode the
-- request body we yield status500.
instance (ApplyHttpPathFn pathSig, FromJSON req, ToJSON resp) => ServeResource pathSig ('HttpPost req resp) where

  type HttpMethodFn pathSig ('HttpPost req resp) = HttpPathFn pathSig req resp

  serveRest ::
    RestSig pathSig ('HttpPost req resp) -> HttpMethodFn pathSig ('HttpPost req resp) -> PartialApplication
  serveRest _ fn = PA $ \request ->
    let path = pathInfo request
    in do
      act <- applyHttpPathFn (Proxy :: Proxy pathSig) path fn :: Maybe (req -> IO (ResponseType resp))
      when (requestMethod request /= methodPost) Nothing
      return $ \respondC -> do
        reqBS <- lazyRequestBody request
        case decode reqBS of
          Just req -> do
            (status, headers, res) <- act req
            respondC $ responseLBS status headers (encode res)
          Nothing -> respondC $ responseLBS status500 [] "Unable to decode request. (JSON)"
