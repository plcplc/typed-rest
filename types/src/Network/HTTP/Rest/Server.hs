{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
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

  -- Pattern matching types and methods
  PartialApplicationPat(..),
  MatchResource(..),
  BodyPat(..),
  PathPat,
  guardMethod,
  pPat,
  bodyDecodePat,
  bodyPat

  ) where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Typeable
import Data.Text (Text, pack)
import GHC.TypeLits
import Network.HTTP.Types

import Network.HTTP.Rest.Signature
import Network.HTTP.Rest.Match

data BodyPat = BodyMissing | BodyPresent | BodyDecodableAs Text
  deriving (Show, Eq)

-- | A type that indicates how well a request matches a resource
data MatchResource =
    MatchMethod (Expected StdMethod StdMethod)
  | MatchBody (Expected BodyPat ())
  | MatchPath PathPat
  deriving (Eq, Show)

-- | The pattern expectation of path components.
type PathPat = [Expected (Maybe Text) (Maybe Text)]

-- | Pattern function for expressing the expectation of a path component. 
pPat ::
  Show (MyProxy comp) =>
  Proxy (comp :/: rest) -> Maybe Text -> Bool -> PathPat
pPat (_ :: Proxy (comp :/: rest)) txt sat
  = [Expected (Just . pack . show $ (MyProxy :: MyProxy comp)) txt sat]

guardMethod :: StdMethod -> StdMethod -> Match [MatchResource] ()
guardMethod expectM actualM = guard' ((:[]) . MatchMethod . Expected expectM actualM) (expectM == actualM) 

bodyPat :: BodyPat -> Bool -> [MatchResource]
bodyPat bp = ((:[]) . MatchBody . Expected bp ())

bodyDecodePat :: PayloadEncoding enc => Proxy enc -> Bool -> [MatchResource]
bodyDecodePat encPx = bodyPat (BodyDecodableAs $ payloadMimeType encPx)

newtype PartialApplicationPat = PAP [MatchResource]
  deriving Eq

instance Show PartialApplicationPat where
  show (PAP matchRes) = unlines $ map show matchRes

-- | A composable wrapper for serving 'RestSig' server functions. The only
-- requirement for composability is that each served signature shares the
-- same encoded representation.
newtype PartialApplication f repr =
  PA { unPA :: StdMethod -> [Text] -> Maybe repr -> Match [PartialApplicationPat] (f repr) }

instance Monoid (PartialApplication f repr) where
  mempty = PA $ \_ _ _ -> matchFail (const [])

  mappend (PA pa1) (PA pa2)
    = PA $ \m p b -> matchCase [pa1 m p b, pa2 m p b]

-- | A type class for applying functions that correspond to a 'RestSig' of
-- a request, pattern matching on the path to the resource.
class ServePath (path :: HttpPathKind) where

  type ServePathFn path result :: *

  servePath ::
       Proxy path
    -> [Text]
    -> Match PathPat (ServePathFn path result)
    -> Match PathPat result

instance ServePath Nil where

  type ServePathFn Nil result = result

  servePath _ ps fn
    = guard' (\g -> map (\x -> Expected Nothing (Just x) g) ps) ([]==ps)
      *> fn

instance
  (ServePath rest,
   KnownSymbol path)
  => ServePath (S path :/: rest) where

  type ServePathFn (S path :/: rest) result = ServePathFn rest result

  servePath ::
       Proxy (S path :/: rest)
    -> [Text]
    -> Match PathPat (ServePathFn (S path :/: rest) result)
    -> Match PathPat result

  servePath pPx (p:ps) fn
    = guard' (pPat pPx (Just p)) (sText (pathHead pPx) == p)
      *> servePath (pathTail pPx) ps fn

  servePath pPx [] fn
    = matchFail (pPat pPx Nothing)
      *> servePath (pathTail pPx) [] fn

instance
  (HttpPathArgument a,
   KnownSymbol path,
   Typeable a,
   ServePath rest)
  => ServePath (A path a :/: rest) where

  type ServePathFn (A path a :/: rest) result = a -> ServePathFn rest result

  servePath ::
       Proxy (A path a :/: rest)
    -> [Text]
    -> Match PathPat (ServePathFn (A path a :/: rest) result)
    -> Match PathPat result

  servePath pPx (p:ps) fn
    = servePath (pathTail pPx) ps (fn <*> match (pPat pPx (Just p)) (fromPathArg p))

  servePath pPx [] fn
    = servePath (pathTail pPx) [] (fn <*> matchFail (pPat pPx Nothing))

-- | A type class for applying functions that correspond to a 'RestSig' of
-- a request, pattern matching on the request method and associated data.
class ServeMethod (method :: HttpMethodKind) encoding where

  type ServeMethodFn (f :: * -> *) method :: *

  serveMethod :: Functor f =>
       Proxy method
    -> Proxy encoding
    -> StdMethod                    -- Request method
    -> Maybe (EncodedRepr encoding) -- Request body
    -> Match [MatchResource] (ServeMethodFn f method)
    -> Match [MatchResource] (f (EncodedRepr encoding))

instance
  (PayloadEncoding encoding,
   Encoder encoding result)
  => ServeMethod (HttpGet result) encoding where

  type ServeMethodFn f (HttpGet result) = f result

  serveMethod :: Functor f =>
       Proxy (HttpGet result)
    -> Proxy encoding
    -> StdMethod
    -> Maybe (EncodedRepr encoding)
    -> Match [MatchResource] (ServeMethodFn f (HttpGet result))
    -> Match [MatchResource] (f (EncodedRepr encoding))

  serveMethod _ encPx method body x
    =  guardMethod GET method
    *> guard' (bodyPat BodyMissing) (isNothing body)
    *> ((payloadEncode encPx <$>) <$> x)

instance
  (PayloadEncoding encoding,
   Encoder encoding body,
   Encoder encoding result)
  => ServeMethod (HttpPost body result) encoding where

  type ServeMethodFn f (HttpPost body result) = body -> f result

  serveMethod :: Functor f =>
       Proxy (HttpPost body result)
    -> Proxy encoding
    -> StdMethod
    -> Maybe (EncodedRepr encoding)
    -> Match [MatchResource] (ServeMethodFn f (HttpPost body result))
    -> Match [MatchResource] (f (EncodedRepr encoding))

  serveMethod _ encPx method body fn
    = guardMethod POST method
      *> ((payloadEncode encPx <$>)
          <$> (fn <*> (do
                body' <- match (bodyPat BodyPresent) body
                match (bodyDecodePat encPx) $ payloadDecode encPx body'
              )))

class (ServePath path, ServeMethod method encoding)
  => Serve path method encoding where

instance (ServePath path, ServeMethod method encoding)
  => Serve path method encoding where

-- | Pattern match a signature on an incoming request, represented by
-- a method, path and optional request body.
serveRequest :: Functor f =>
  (Serve path method encoding)
  => Proxy (RestSig path method encoding)
  -> StdMethod
  -> [Text]
  -> Maybe (EncodedRepr encoding)
  -> ServePathFn path (ServeMethodFn f method)
  -> Match [MatchResource] (f (EncodedRepr encoding))

serveRequest (_ :: Proxy (RestSig path method encoding)) method ps body
  = serveMethod (Proxy :: Proxy method) (Proxy :: Proxy encoding) method body
  . mapMatch ((:[]) . MatchPath)
  . servePath (Proxy :: Proxy path) ps
  . pure

serve :: Functor f =>
  (Serve path method encoding)
  => Proxy (RestSig path method encoding)
  -> ServePathFn path (ServeMethodFn f method)
  -> PartialApplication f (EncodedRepr encoding)
serve sigPx fn = PA $ \method path body -> mapMatch ((:[]) . PAP) $ serveRequest sigPx method path body fn
