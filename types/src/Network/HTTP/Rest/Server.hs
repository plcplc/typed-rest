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

  -- * The main interface to this module
  serve,

  -- * Types and type classes
  ServePath(..),
  ServeMethod(..),
  Serve,
  PartialApplication(..),

  ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Proxy
import Data.Text (Text, pack)
import GHC.TypeLits
import Network.HTTP.Types

import Network.HTTP.Rest.Signature

-- | A type class for applying functions that correspond to a 'RestSig' of
-- a request, pattern matching on the path to the resource.
class ServePath (path :: HttpPathKind) where

  type ServePathFn path result :: *

  servePath :: Proxy path -> [Text] -> ServePathFn path result -> Maybe result

instance ServePath Nil where

  type ServePathFn Nil result = result

  servePath _ [] fn = Just fn
  servePath _ _  _  = Nothing

instance
  (KnownSymbol path,
   ServePath rest)
  =>ServePath (S path :/: rest) where

  type ServePathFn (S path :/: rest) result = ServePathFn rest result

  servePath ::
       Proxy (S path :/: rest)
    -> [Text]
    -> ServePathFn (S path :/: rest) result
    -> Maybe result

  servePath _ (p:ps) fn | p == pack (symbolVal (Proxy :: Proxy path))
    = servePath (Proxy :: Proxy rest) ps fn
  servePath _ _      _
    = Nothing

instance
  (HttpPathArgument a,
   ServePath rest)
  => ServePath (A path a :/: rest) where

  type ServePathFn (A path a :/: rest) result = a -> ServePathFn rest result

  servePath ::
       Proxy (A path a :/: rest)
    -> [Text]
    -> ServePathFn (A path a :/: rest) result
    -> Maybe result

  servePath _ (p:ps) fn | Just x <- fromPathArg p
    = servePath (Proxy :: Proxy rest) ps (fn x)
  servePath _ _      _
    = Nothing

-- | A type class for applying functions that correspond to a 'RestSig' of
-- a request, pattern matching on the request method and associated data.
class ServeMethod (method :: HttpMethodKind) encoding where

  type ServeMethodFn method :: *

  serveMethod ::
       Proxy method
    -> Proxy encoding
    -> StdMethod
    -> Maybe (EncodedRepr encoding)
    -> ServeMethodFn method
    -> Maybe (EncodedRepr encoding)

instance (PayloadEncoding encoding result)
  => ServeMethod (HttpGet result) encoding where

  type ServeMethodFn (HttpGet result) = result

  serveMethod ::
       Proxy (HttpGet result)
    -> Proxy encoding
    -> StdMethod
    -> Maybe (EncodedRepr encoding)
    -> ServeMethodFn (HttpGet result)
    -> Maybe (EncodedRepr encoding)

  serveMethod _ _ method body x | method == GET, Nothing <- body
    = Just (payloadEncode (Proxy :: Proxy encoding) x)
  serveMethod _ _ _      _    _
    = Nothing

instance (PayloadEncoding encoding result, PayloadEncoding encoding body)
  => ServeMethod (HttpPost body result) encoding where

  type ServeMethodFn (HttpPost body result) = body -> result

  serveMethod ::
       Proxy (HttpPost body result)
    -> Proxy encoding
    -> StdMethod
    -> Maybe (EncodedRepr encoding)
    -> ServeMethodFn (HttpPost body result)
    -> Maybe (EncodedRepr encoding)

  serveMethod _ _ method bodyMaybe fn | method == POST, Just body <- bodyMaybe
    = payloadEncode (Proxy :: Proxy encoding) . fn <$> payloadDecode (Proxy :: Proxy encoding) body
  serveMethod _ _ _      _    _
    = Nothing

class (ServePath path, ServeMethod method encoding)
  => Serve path method encoding where

instance (ServePath path, ServeMethod method encoding)
  => Serve path method encoding where

-- | Pattern match a signature on an incoming request, represented by
-- a method, path and optional request body.
serveRequest ::
  (Serve path method encoding)
  => Proxy (RestSig path method encoding)
  -> StdMethod
  -> [Text]
  -> Maybe (EncodedRepr encoding)
  -> ServePathFn path (ServeMethodFn method)
  -> Maybe (EncodedRepr encoding)

serveRequest (_ :: Proxy (RestSig path method encoding)) method path body =
  servePath (Proxy :: Proxy path) path
  >=> serveMethod (Proxy :: Proxy method) (Proxy :: Proxy encoding) method body

-- | A composable wrapper for serving 'RestSig' server functions. The only
-- requirement for composability is that each served signature shares the
-- same encoded representation.
newtype PartialApplication repr = PA { unPA :: StdMethod -> [Text] -> Maybe repr -> Maybe repr }

instance Monoid (PartialApplication repr) where
  mempty = PA $ \_ _ _ -> Nothing
  mappend (PA pa1) (PA pa2) = PA $ \method path body -> getFirst $
    First (pa1 method path body)
    <> First (pa2 method path body)

serve ::
  (Serve path method encoding)
  => Proxy (RestSig path method encoding)
  -> ServePathFn path (ServeMethodFn method)
  -> PartialApplication (EncodedRepr encoding)
serve sigPx fn = PA $ \method path body -> serveRequest sigPx method path body fn
