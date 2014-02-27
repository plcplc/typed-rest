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
-- | This module defines the machinery for serving typed REST API signatures.
module Network.HTTP.Rest.Server (

  -- * Serving Resource Signatures
  ApplyHttpPathFn(..),
  ResponseType,
  ServeResource(..),

  ) where

import GHC.TypeLits

import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status)
import Network.Wai (Application, Request(..), lazyRequestBody, responseLBS)

import Data.Aeson (FromJSON,ToJSON, decode, encode)
import Data.Text (Text, pack)
import Data.Typeable

import Network.HTTP.Rest.Signature

-- | A response type that manually encodes every aspect of a http response. I'm
-- hoping to replace this with some more type structure at the RestSig level.
type ResponseType a = (Status, ResponseHeaders, a)

-- | Serving of REST resources is encapsulated in this typeclass. (Necessary
-- because we need different cases for each type of HttpMethod?)
class (SingI path) => ApplyHttpPathFn (path :: HttpPathKind {- RestSig path method -}) where

  -- | A type family for translating HttpPathKind types with response types to function types.
  type HttpPathFn path req resp :: *

  -- | A function to apply 'HttpPathFn's to the path they match.
  applyHttpPathFn :: Sing path -> [Text] -> req -> HttpPathFn path req resp -> Maybe (IO (ResponseType resp))

-- The ApplyHttpPathFn for empty paths.
instance ApplyHttpPathFn Nil where

  type HttpPathFn Nil req resp = req -> IO (ResponseType resp)

  applyHttpPathFn :: Sing Nil -> [Text] -> req -> HttpPathFn Nil req resp -> Maybe (IO (ResponseType resp))
  applyHttpPathFn _ [] req fn = Just (fn req)
  applyHttpPathFn _ _  _   _ = Nothing

-- The ApplyHttpPathFn for non-empty paths that pass arguments to the given HttpPathFn.
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

-- The ApplyHttpPathFn for non-empty paths that do not pass arguments to the given HttpPathFn.
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

-- | A typeclass to wrap 'HttpPathFn's in 'Application's.
class (ApplyHttpPathFn pathSig) => ServeResource (pathSig :: HttpPathKind) (method :: HttpMethodKind) where

  type HttpMethodFn pathSig method :: *

  -- | A function that wraps a 'HttpPathFn' in an 'Application' (?). For requests
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
