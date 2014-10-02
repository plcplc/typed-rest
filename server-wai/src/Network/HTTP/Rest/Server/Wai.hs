{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
-- | This module defines the Wai-interface to typed-rest.
module Network.HTTP.Rest.Server.Wai ( runPartialApplication ) where

import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types
import Network.Wai

import Network.HTTP.Rest.Server

-- | Run a 'PartialApplication'. It returns 404 if the 'PartialApplication'
-- doesn't match.
runPartialApplication :: PartialApplication LBS.ByteString-> Application
runPartialApplication (PA pa) req respond = do
  body <- lazyRequestBody req
  let bodyMaybe = maybe Nothing (const $ Just body) (LBS.uncons body)
  let path = pathInfo req
  let method = either (\m -> error $ "Only StdMethods supported. (Got " ++ show m ++ ")")
                      id (parseMethod $ requestMethod req)

  case pa method path bodyMaybe of
    Just res -> respond $ responseLBS status200 [] res
    Nothing -> respond $ responseLBS status404 [] "No resources match the request"
