{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
-- | This module defines typed REST API signatures, common for both servers and
-- clients.
module Network.HTTP.Rest.Signature (

  -- * REST Resource Signatures.
  RestSig(..),
  HttpMethodKind(..),
  HttpPathKind(..),
  PathComponentKind(..),
  HttpPathArgument(..),
  PayloadEncoding(..),

  ) where

import Data.Text (Text, pack, unpack)
import Data.Typeable
import GHC.TypeLits

-- | 'Proxy' defines its own, useless 'Show' instance :-(.
data MyProxy (x :: k) = MyProxy

-- | A data type representing the signature of a single REST resource. It uses
-- the datatype 'HttpMethodKind' promoted to a kind to express a type-list of accepted
-- http methods.
-- Note that @method :: HttpMethodKind@ and @path :: HttpPathKind@.
data RestSig :: HttpPathKind -> HttpMethodKind -> * -> * where
  RestResource :: RestSig path method encoding

-- | A datatype representing a Http method. It's used promoted to a kind in its use in 'RestSig'.
data HttpMethodKind where
  HttpGet     :: response -> HttpMethodKind
  HttpPost    :: request -> response -> HttpMethodKind
  HttpPut     :: request -> response -> HttpMethodKind
  HttpDelete  :: response -> HttpMethodKind

-- | A HttpPath used exclusively as a Kind, to be able to specify resource urls as types only.
--
-- (constructor doc, to be placed appropriately pending haddock ticket #43):
--
-- [@component :/: rest@] Path composition, infix and right associative, with precedence 4,
-- which is low enough to not require parentheses when applied with 'A' and 'S'.
--
-- [@Nil@] Terminate a http path.
--
data HttpPathKind where
  (:/:) :: PathComponentKind -> HttpPathKind -> HttpPathKind
  Nil :: HttpPathKind

infixr 4 :/:

-- | The kind containing types of 'HttpPathKind' components. Components may be
-- either literals or variables with a type.
--
-- (constructor doc, to be placed appropriately pending haddock ticket #43):
--
-- [@S \"symbol\"@] A constant literal path component.
--
-- [@A \"symbol\" type@] A variable path component.
data PathComponentKind where
  S :: Symbol -> PathComponentKind
  A :: Symbol -> (a :: *) -> PathComponentKind

instance (KnownSymbol a) => Show (MyProxy (S a)) where
 show (MyProxy :: MyProxy (S a)) = symbolVal (MyProxy :: MyProxy a)

instance (KnownSymbol a, Typeable x) => Show (MyProxy (A a (x :: *))) where
 show (MyProxy :: MyProxy (A a x)) = "{" ++ symbolVal (MyProxy :: MyProxy a) ++ " :: " ++ show (typeOf (undefined :: x)) ++ "}"

instance (Show (MyProxy c), Show (MyProxy httpPath)) => Show (MyProxy (c :/: httpPath)) where
  show (MyProxy :: MyProxy (c :/: httpPath)) = show (MyProxy :: MyProxy c) ++ "/" ++ show (MyProxy :: MyProxy httpPath)

instance Show (MyProxy Nil) where
  show _ = ""

-- | To properly serialise HttpPathFn path arguments we need a dedicated
-- typeclass, as opposed to just Show or ToJson. Instances of this class should
-- take care to obey the restrictions of resource paths in Http.
-- (Alternatively we could just urlencode the output of 'Show')
class HttpPathArgument a where

  -- | Serialize a path argument.
  toPathArg :: a -> Text

  -- | Deserialize a path argument.
  fromPathArg :: Text -> Maybe a

-- | This instance relies on 'Show'. Since Data.Text is all about efficiency
-- due to absence of list-based strings, it would be defeating to export this
-- instance in its current form.
instance HttpPathArgument Int where

  toPathArg = pack . show

  fromPathArg txt = case reads $ unpack txt of
    [(i, "")] -> Just i
    _         -> Nothing

-- | A type class for specifying request/response payload encodings.
class PayloadEncoding enc a where

  type EncodedRepr enc :: *

  payloadDecode   :: Proxy enc -> EncodedRepr enc -> Maybe a
  payloadEncode   :: Proxy enc -> a -> EncodedRepr enc
  payloadMimeType :: Proxy enc -> Text
