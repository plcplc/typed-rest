-- | Auxiliary module to aeson that provides a convenient alternative to
-- 'decode' that disregards the defining JSON RFC-4627
-- object/array-at-toplevel restriction.
module Data.Aeson.NonRFC where

import Data.Aeson
import Data.Aeson.Parser (value')
import Data.ByteString.Lazy as LBS
import Data.Attoparsec.ByteString (maybeResult, parse)

-- | RFC-4627 sucks cocks in hell. We have to go all the way around
-- Data.Aeson and use the low-level attoparsec tools to get beyond the
-- only-objects-at-top-level restriction. Why on earth would you sacrifice
-- having encode/decode an isomorphism just to comply with an RFC?.
-- Sorry potential readers, I'm just venting steam...
parseNonRfcJson :: FromJSON a => LBS.ByteString -> a
parseNonRfcJson lbs = let
  bs          = LBS.toStrict lbs
  Just val    = maybeResult $ parse value' bs
  Success res = fromJSON val
  in res
