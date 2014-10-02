{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.HTTP.Rest.Encoding.JSON ( JSONEncoding ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Data.Aeson.NonRFC

import Network.HTTP.Rest.Signature

-- | Encoding marker for PayloadEncoding.
data JSONEncoding

instance (FromJSON a, ToJSON a) => PayloadEncoding JSONEncoding a where

  type EncodedRepr JSONEncoding = LBS.ByteString

  payloadDecode _ = parseNonRfcJson
  payloadEncode _ = encode
  payloadMimeType _ = "application/json"