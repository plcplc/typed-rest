{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
-- | This module defines the Wai-interface to typed-rest.
module Network.HTTP.Rest.Server.Wai
  where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.String
import Data.Monoid
import Network.HTTP.Types
import Network.Wai

import Network.HTTP.Rest.Match
import Network.HTTP.Rest.Server

-- | A counterpart of PartialApplications, as expressed conveniently in Wai
-- terms.
newtype PartialWaiApplication = PWA {
  unPWA :: Request -> IO (Match [MatchHttp] ((Response -> IO ResponseReceived) -> IO ResponseReceived))
  }

instance Monoid PartialWaiApplication where

  mempty = PWA $ \_ -> return $ matchFail (const [])

  mappend (PWA pwa1) (PWA pwa2) = PWA $ \req ->
    matchCase <$> sequence [pwa1 req, pwa2 req]

data MatchHttp = MatchStdMethod (Expected StdMethodPat BS.ByteString)
               | MatchResource [PartialApplicationPat]
  deriving (Eq, Show)

data StdMethodPat = IsStdMethod
  deriving (Eq, Show)

-- | Lift a 'PartialApplication' to a 'PartialWaiApplication'.
liftPA :: PartialApplication IO LBS.ByteString -> PartialWaiApplication
liftPA (PA pa) = PWA $ \req -> do
  body <- lazyRequestBody req
  return $ do
    let bodyMaybe = maybe Nothing (const $ Just body) (LBS.uncons body)
    let path = filter (/="") $ pathInfo req
    method <- either
                (\m -> matchFail ((:[]) . MatchStdMethod . Expected IsStdMethod m))
                return
                $ parseMethod (requestMethod req)
    res    <- mapMatch ((:[]) . MatchResource) $ pa method path bodyMaybe
    return $ \respond -> res >>= respond . (responseLBS status200 [])

-- | Run a PartialApplication, yielding "HTTP 404" if no resources match
-- the request.
runPWA :: PartialWaiApplication -> Application
runPWA pwa req respond = do
  matched <- unPWA pwa req
  case matched of
    MatchSuccess _ res -> res respond
    MatchErr e -> notFound e

  where
    notFound e = respond $ responseLBS status404 [] (fromString $ show e)

-- | Lift a Wai 'Application' to a 'PartialWaiApplication' by putting a guard
-- predicate in front of it.
guardApp :: (Request -> IO Bool) -> Application -> PartialWaiApplication
guardApp p app = PWA $ \req -> do
  guard <- p req
  return $
    if guard
      then return $ \respond -> app req respond
      else matchFail mempty -- Really, there should be a 'error' type parameter to 'PartialWaiApplication'.
