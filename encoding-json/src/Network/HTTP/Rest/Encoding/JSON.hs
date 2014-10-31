{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Rest.Encoding.JSON ( JSONEncoding ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Data.Aeson.NonRFC
import Data.Text ()

import Network.HTTP.Rest.Signature

-- | Encoding marker for PayloadEncoding.
data JSONEncoding

instance PayloadEncoding JSONEncoding where

  type Encoder JSONEncoding a = (FromJSON a, ToJSON a)
  type EncodedRepr JSONEncoding = LBS.ByteString

  payloadDecode _ = parseNonRfcJson
  payloadEncode _ = encode
  payloadMimeType _ = "application/json"
