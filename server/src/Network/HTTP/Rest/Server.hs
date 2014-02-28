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
-- | This module defines the machinery for serving typed REST API signatures,
-- by utilising the Wai server infrastructure. It is capable of wrapping
-- resource serving functions (with type signatures corresponding to given
-- 'RestSig's) as Wai middleware.
module Network.HTTP.Rest.Server (

  -- * Serving Resource Signatures
  ServeResource(..),
  ApplyHttpPathFn(..),
  ResponseType

  ) where

import GHC.TypeLits

import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status)
import Network.Wai (Application, Request(..), lazyRequestBody, responseLBS)

import Data.Aeson (FromJSON,ToJSON, decode, encode)
import Data.Text (Text, pack)
import Data.Typeable

import Network.HTTP.Rest.Signature

-- | A response type that manually encodes aspects of a http response.
type ResponseType a = (Status, ResponseHeaders, a)

-- | A typeclass to apply resource-serving functions, 'HttpPathFn', to slightly
-- decomposed requests (consisting of a list of path components and a
-- deserialised request payload), giving 'Nothing' if the request path did not
-- match the signature. Note that 'path :: HttpPathKind'.
class (SingI path) => ApplyHttpPathFn (path :: HttpPathKind) where

  -- | A type family for translating HttpPathKind types with response types to
  -- function types.
  type HttpPathFn path req resp :: *

  -- | A function to apply 'HttpPathFn's to the path they match.
  applyHttpPathFn :: Sing path -> [Text] -> req -> HttpPathFn path req resp -> Maybe (IO (ResponseType resp))

-- | The 'ApplyHttpPathFn' case for empty paths.
instance ApplyHttpPathFn Nil where

  type HttpPathFn Nil req resp = req -> IO (ResponseType resp)

  applyHttpPathFn :: Sing Nil -> [Text] -> req -> HttpPathFn Nil req resp -> Maybe (IO (ResponseType resp))
  applyHttpPathFn _ [] req fn = Just (fn req)
  applyHttpPathFn _ _  _   _ = Nothing

-- | The 'ApplyHttpPathFn' case for variable path components (that pass arguments to the given 'HttpPathFn').
instance
  (ApplyHttpPathFn rest,
  SingI pathSig,
  Typeable a,
  HttpPathArgument a) =>
    ApplyHttpPathFn (A pathSig a :/: rest) where

  type HttpPathFn (A pathSig a :/: rest) req resp = a -> HttpPathFn rest req resp

  applyHttpPathFn ::
    Sing (A pathSig a :/: rest) -> [Text] -> req ->
    HttpPathFn (A pathSig a :/: rest) req resp -> Maybe (IO (ResponseType resp))

  applyHttpPathFn _ (path:pathRest) req fn = let
    pathSig' = fromSing (sing :: Sing pathSig) :: String
    argMaybe = fromPathArg path
    in case (path == pack pathSig', argMaybe) of
      (True, Just arg) -> applyHttpPathFn (sing :: Sing rest) pathRest req (fn arg)
      _                -> Nothing
  applyHttpPathFn _ [] _ _ = Nothing

-- | The 'ApplyHttpPathFn' case for constant literal path components (that do not pass arguments to the given 'HttpPathFn').
instance
  (ApplyHttpPathFn rest, SingI pathSig) =>
  ApplyHttpPathFn (S pathSig :/: rest) where

  type HttpPathFn (S pathSig :/: rest) req resp = HttpPathFn rest req resp

  applyHttpPathFn ::
    Sing (S pathSig :/: rest) -> [Text] -> req ->
    HttpPathFn (S pathSig :/: rest) req resp -> Maybe (IO (ResponseType resp))

  applyHttpPathFn _ (path:pathRest) req fn = let
    pathSig' = fromSing (sing :: Sing pathSig) :: String
    in case path == pack pathSig' of
      True -> applyHttpPathFn (sing :: Sing rest) pathRest req fn
      _    -> Nothing
  applyHttpPathFn _ [] _ _ = Nothing

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
  serveRest :: (SingI pathSig) =>
    RestSig pathSig method -> HttpMethodFn pathSig method -> Application -> Application

-- | The instance for serving GET requests.
instance (ApplyHttpPathFn pathSig, ToJSON resp) => ServeResource pathSig ('HttpGet resp) where

  type HttpMethodFn pathSig ('HttpGet resp) = HttpPathFn pathSig () resp

  serveRest :: (SingI pathSig) =>
    RestSig pathSig ('HttpGet resp) -> HttpMethodFn pathSig ('HttpGet resp) -> Application -> Application
  serveRest _ fn fallback request = do
    let path = pathInfo request
    case applyHttpPathFn (sing :: Sing pathSig) path () fn :: Maybe (IO (ResponseType resp)) of
      Just act -> do
        (status, headers, res) <- act
        -- A bit crude:
        return $ responseLBS status headers (encode res)
      Nothing -> fallback request

-- | The instance for serving POST requests.
instance (ApplyHttpPathFn pathSig, FromJSON req, ToJSON resp) => ServeResource pathSig ('HttpPost req resp) where

  type HttpMethodFn pathSig ('HttpPost req resp) = HttpPathFn pathSig req resp

  serveRest :: (SingI pathSig) =>
    RestSig pathSig ('HttpPost req resp) -> HttpMethodFn pathSig ('HttpPost req resp) -> Application -> Application
  serveRest _ fn fallback request = do
    let path = pathInfo request
    reqBS <- lazyRequestBody request
    case decode reqBS :: Maybe req of
      Just req ->
        case applyHttpPathFn (sing :: Sing pathSig) path req fn :: Maybe (IO (ResponseType resp)) of
          Just act -> do
            (status, headers, res) <- act
            -- A bit crude:
            return $ responseLBS status headers (encode res)
          Nothing -> fallback request
      Nothing -> fallback request
