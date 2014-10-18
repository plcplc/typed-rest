{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
-- | This module defines the Wai-interface to typed-rest.
module Network.HTTP.Rest.Server.Wai
  where

import qualified Data.ByteString.Lazy as LBS
import Data.Monoid
import Network.HTTP.Types
import Network.Wai

import Network.HTTP.Rest.Server

-- | A counterpart of PartialApplications, as expressed conveniently in Wai
-- terms.
newtype PartialWaiApplication = PWA {
  unPWA :: Request -> IO (Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived))
  }

instance Monoid PartialWaiApplication where
  mempty  = PWA $ \_ -> return Nothing
  mappend (PWA pwa1) (PWA pwa2) = PWA $ \req -> do
    res1 <- pwa1 req
    case res1 of
      Nothing -> pwa2 req
      _       -> return res1

-- | Lift a 'PartialApplication' to a 'PartialWaiApplication'.
liftPA :: PartialApplication IO LBS.ByteString -> PartialWaiApplication
liftPA (PA pa) = PWA $ \req -> do
  body <- lazyRequestBody req
  return $ do
    let bodyMaybe = maybe Nothing (const $ Just body) (LBS.uncons body)
    let path = pathInfo req
    method <- either (const Nothing) Just (parseMethod $ requestMethod req)
    res <- pa method path bodyMaybe
    return $ \respond -> res >>= respond . (responseLBS status200 [])

-- | Run a PartialApplication, yielding "HTTP 404" if no resources match
-- the request.
runPWA :: PartialWaiApplication -> Application
runPWA pwa req respond = do
  maybe notFound ($ respond) =<< unPWA pwa req

  where
    notFound = respond $ responseLBS status404 [] "No resources match the request"

-- | Lift a Wai 'Application' to a 'PartialWaiApplication' by putting a guard
-- predicate in front of it.
guardApp :: (Request -> IO Bool) -> Application -> PartialWaiApplication
guardApp p app = PWA $ \req -> do
  guard <- p req
  return $
    if guard
      then Just $ \respond -> app req respond
      else Nothing
