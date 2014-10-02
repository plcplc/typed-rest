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

  -- * The main interface to the client
  requestResource,

  -- * Types
  HttpClientResult(..),

  -- * Typeclasses for requesting resources
  Request,
  RequestPath(..),
  RequestMethod(..),

  -- * Internal functions, exposed for testing mainly
  toPathComponents,
  fromPathComponents,
  buildUrl

  ) where

import Data.Proxy
import Data.Text hiding (filter, foldr)

import GHC.TypeLits

import Network.HTTP.Types

import Network.HTTP.Rest.Signature

data HttpClientResult a = ResponseNoParse | Success a
  deriving (Eq, Show)

toPathComponents :: String -> [String]
toPathComponents = filter (/=[]) . foldr splitPath []
  where
    splitPath '/' ps      = []:ps
    splitPath c   (p:ps)  = (c:p):ps
    splitPath c   []      = [[c]]

fromPathComponents :: [String] -> String
fromPathComponents [] = "/"
fromPathComponents (p:ps) = '/': (p ++ fromPathComponents ps)

-- | Convenience wrapper around 'requestPath' for urls without a prefix.
-- (initial "/" is added automatically)
buildUrl :: RequestPath path => Proxy path -> RequestPathFn path String
buildUrl p = requestPath p fromPathComponents []

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

-- | Type class to construct a function that constructs Requests based on
-- a 'method :: HttpMethodKind'.
class RequestMethod (method :: HttpMethodKind) encoding where

  type RequestMethodFn (m :: * -> *) method :: *

  requestMethod :: Monad m =>
       Proxy method
    -> Proxy encoding
    -> (StdMethod -> Maybe (EncodedRepr encoding) -> m (EncodedRepr encoding))
    -> RequestMethodFn m method

-- | The RequestMethod instance for GET requests.
instance (PayloadEncoding encoding resp) => RequestMethod ('HttpGet resp) encoding where

  type RequestMethodFn m ('HttpGet resp) = m (HttpClientResult resp)

  requestMethod _ encodePx act = do
    res <- act GET Nothing
    return $ maybe ResponseNoParse Success $ payloadDecode encodePx res

-- | The RequestResource instance for POST requests.
instance
  (PayloadEncoding encoding body,
  PayloadEncoding encoding resp)
  => RequestMethod ('HttpPost body resp) encoding where

  type RequestMethodFn m ('HttpPost body resp) = body -> m (HttpClientResult resp)

  requestMethod _ payloadPx act body = do
    res <- act POST (Just $ payloadEncode payloadPx body)
    return $ maybe ResponseNoParse Success $ payloadDecode payloadPx res

class
  (RequestPath path, RequestMethod method encoding) =>
  Request (path :: HttpPathKind) (method :: HttpMethodKind) (encoding :: *) where

instance
  (RequestPath path, RequestMethod method encoding) =>
  Request (path :: HttpPathKind) (method :: HttpMethodKind) (encoding :: *) where

requestResource ::
  --(RequestPath path, RequestMethod method encoding, Functor m, Monad m)
  (Request path method encoding, Monad m)
  => Proxy (RestSig path method encoding)
  -> ([String] -> StdMethod -> Maybe (EncodedRepr encoding) -> m (EncodedRepr encoding))
  -> RequestPathFn path (RequestMethodFn m method)
requestResource (_ :: Proxy (RestSig path method encoding)) act =
  requestPath (Proxy :: Proxy path)
    (requestMethod (Proxy :: Proxy method) (Proxy :: Proxy encoding) . act) []

