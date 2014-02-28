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
  requestResource,
  requestResourceDef,

  -- * Typeclasses for requesting resources.
  RequestResource(..),
  RequestBuilder(..)

  ) where

import Control.Applicative

import Data.Aeson (FromJSON,ToJSON, decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Maybe
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import GHC.TypeLits

import Network.HTTP.Client
  (defaultManagerSettings, httpLbs, Manager, newManager,
  Request(..), RequestBody(..), Response(..), closeManager)

import Network.HTTP.Types.Method (methodGet, methodPost)

import Network.HTTP.Rest.Signature

prependReqPath :: Text -> Request -> Request
prependReqPath pathElem req = req {
  path = BS.concat $
    (encodeUtf8 pathElem)
    :(if BS.empty == path req
        then []
        else [encodeUtf8 $ pack "/", path req])
    }

-- | Typeclass that enables construction of a Request that matches a given HttpPath.
-- Note that @path :: HttpPathKind@.
class (SingI path) => RequestBuilder (path :: HttpPathKind) where

  -- | An associated type that gives the function type that corresponds to the
  -- HttpPath. This means an argument of type @a@ for every @A \"symbol\" a@
  -- component.
  type RequestPathSig path resp :: *

  -- | Guided by a HttpPath, and given a continuation on the final request, we
  -- have a function that translates each of the variable components of @path@
  -- to function arguments.
  requestBuilder :: Sing path -> (Request -> resp) -> RequestPathSig path resp

-- | The RequestBuilder case for empty http paths.
instance (SingI 'Nil) => RequestBuilder 'Nil where

  -- The case for 'Nil is just Request - no more path segments to construct.
  type RequestPathSig 'Nil resp = resp

  requestBuilder :: Sing 'Nil -> (Request -> resp) -> RequestPathSig 'Nil resp
  requestBuilder _ k = k $ def { path = "" }

-- | The RequestBuilder case for literal http path components.
instance (
  SingI (S pathSig :/: rest),
  SingI (S pathSig),
  SingI rest,
  RequestBuilder rest)
  => RequestBuilder (S pathSig :/: rest) where

  -- The case for @S sig :/: rest@ reduces to @rest@.
  type RequestPathSig (S pathSig :/: rest) resp = RequestPathSig rest resp

  requestBuilder ::
       Sing (S pathSig :/: rest)
    -> (Request -> resp)
    -> RequestPathSig (S pathSig :/: rest) resp

  requestBuilder _ k = requestBuilder (sing :: Sing rest) (k . (prependReqPath $ pack $ fromSing (sing :: Sing (S pathSig))))

-- | The RequestBuilder case for variable http path components.
instance (
  SingI (A pathSig a :/: rest),
  SingI (A pathSig a),
  HttpPathArgument a,
  SingI rest,
  RequestBuilder rest)
  => RequestBuilder (A pathSig a :/: rest) where

  -- The case for 'A sig a :/: rest' gives an actual function.
  type RequestPathSig (A pathSig a :/: rest) resp = a -> RequestPathSig rest resp

  requestBuilder :: Sing (A pathSig a :/: rest) -> (Request -> resp) -> RequestPathSig (A pathSig a :/: rest) resp
  requestBuilder _ k x = requestBuilder (sing :: Sing rest) (k . (prependReqPath $ toPathArg x))

-- | Given a 'RestSig' we construct a function that queries the resource through \'http-client\'.
-- Note that @method :: HttpMethodKind@ and @path :: HttpPathKind@.
class RequestResource (path :: HttpPathKind) (method :: HttpMethodKind) where

  -- | Associated type to translate the payload types used in the given method.
  -- For GET requests for instance this reduces to the type of the server
  -- response payload, while POST requests also carry a data payload in
  -- addition to that of the response.
  type RequestMethodSig method :: *

  -- | Given an IO action that actually requests the resource, we give a
  -- function that takes all the request parameters (as given by the path and
  -- method) and performs the request.
  requestResourceK ::
       (Request -> IO (Response LBS.ByteString))
    -> Text
    -> RestSig path method
    -> RequestPathSig path (RequestMethodSig method)

-- | The RequestResource instance for GET requests.
instance
  (SingI path,
  FromJSON resp,
  RequestBuilder path)
  => RequestResource path ('HttpGet resp) where

  type RequestMethodSig ('HttpGet resp) = IO resp

  requestResourceK ::
       (Request -> IO (Response LBS.ByteString))
    -> Text
    -> RestSig path ('HttpGet resp)
    -> RequestPathSig path (RequestMethodSig ('HttpGet resp))

  requestResourceK issuer hostNm _ =
    requestBuilder
      (sing :: Sing path)
      (\req -> decodeResp <$> issuer (encodeRequest req))

    where

      encodeRequest :: Request -> Request
      encodeRequest request = request {
        method = methodGet,
        host = encodeUtf8 hostNm}

      decodeResp :: Response LBS.ByteString -> resp
      decodeResp = fromJust . decode . responseBody

-- | The RequestResource instance for POST requests.
instance
  (SingI path,
  ToJSON req,
  FromJSON resp,
  RequestBuilder path)
  => RequestResource path ('HttpPost req resp) where

  type RequestMethodSig ('HttpPost req resp) = req -> IO resp

  requestResourceK ::
       (Request -> IO (Response LBS.ByteString))
    -> Text
    -> RestSig path ('HttpPost req resp)
    -> RequestPathSig path (RequestMethodSig ('HttpPost req resp))

  requestResourceK issuer hostNm _ =
    requestBuilder
      (sing :: Sing path)
      (\req body -> decodeResp <$> issuer (encodeRequest req body))

    where

      encodeRequest :: Request -> req -> Request
      encodeRequest request body = request {
        method = methodPost,
        host = encodeUtf8 hostNm,
        requestBody = RequestBodyLBS (encode body) }

      decodeResp :: Response LBS.ByteString -> resp
      decodeResp = fromJust . decode . responseBody

-- | Request a resource: @requestResource manager hostname signature
-- \<RequestPathSig arguments\>@. The 'Manager' concept is from the @http-client@ package.
requestResource ::
  RequestResource path method =>
    Manager ->
    Text ->
    RestSig path method ->
    RequestPathSig path (RequestMethodSig method)

requestResource man hostNm sig = requestResourceK (flip httpLbs man) hostNm sig

-- | Request a resource, using the default 'Manager' as defined in @http-client@.
requestResourceDef ::
  RequestResource path method =>
    Text ->
    RestSig path method ->
    RequestPathSig path (RequestMethodSig method)

requestResourceDef hostNm sig = requestResourceK
  (\req -> do
    man <- newManager defaultManagerSettings
    resp <- httpLbs req man
    closeManager man
    return resp) hostNm sig
