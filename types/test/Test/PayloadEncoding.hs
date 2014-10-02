{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Test.PayloadEncoding where

import Data.Dynamic
import Data.Text()

import Network.HTTP.Rest.Signature

data IdentityEncoding

instance Typeable a => PayloadEncoding IdentityEncoding a where

  type EncodedRepr IdentityEncoding = Dynamic

  payloadEncode _   = toDyn
  payloadDecode _   = fromDynamic
  payloadMimeType _ = "nonserializable Data.Dynamic"

identityEncPx :: Proxy IdentityEncoding
identityEncPx = Proxy
