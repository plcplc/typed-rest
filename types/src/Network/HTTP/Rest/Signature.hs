{-# LANGUAGE UndecidableInstances #-}
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
-- | This module defines typed REST API signatures, and machinery for serving them.
module Network.HTTP.Rest.Signature (

  -- * REST Resource Signatures.
  RestSig(..),
  HttpMethodKind(..),
  HttpPathKind(..),
  PathComponentKind(..),
  HttpPathArgument(..),

  ) where

import Data.Text (Text, pack, unpack)
import Data.Typeable
import GHC.TypeLits

-- | A data type representing the signature of a single REST resource. It uses
-- the datatype 'HttpMethodKind' promoted to a kind to express a type-list of accepted
-- http methods.
data RestSig :: HttpPathKind -> HttpMethodKind -> * where
  RestResource :: RestSig a b

-- | A datatype representing a Http method. It's used promoted to a kind in 'RestSig'.
data HttpMethodKind where
  HttpOptions :: HttpMethodKind
  HttpGet     :: response -> HttpMethodKind
  HttpHead    :: HttpMethodKind
  HttpPost    :: request -> response -> HttpMethodKind
  HttpPut     :: request -> response -> HttpMethodKind
  HttpDelete  :: response -> HttpMethodKind
  HttpTrace   :: HttpMethodKind
  HttpConnect :: HttpMethodKind

-- | A HttpPath used exclusively as a Kind, to be able to specify resource urls as types only.
data HttpPathKind where

  -- (doc pending haddock ticket #43)
  -- . | Path composition, infix and right associative, with precedence 4,
  -- which is low enough to not require parentheses when applied with 'A' and 'S'.
  (:/:) :: PathComponentKind -> HttpPathKind -> HttpPathKind

  -- (Same as above)
  -- . | Terminate a http path.
  Nil :: HttpPathKind

infixr 4 :/:

-- | The kind containing types of HttpPathKind components. Components may be either constants or variables.
data PathComponentKind where
  -- (doc pending haddock ticket #43)
  -- . | A constant component.
  S :: Symbol -> PathComponentKind

  -- (doc pending haddock ticket #43)
  -- . | A variable component.
  A :: Symbol -> a -> PathComponentKind

-- | Make all types of PathComponentKind reflectable as values. For now we just
-- reflect to Strings, so our Sing-suite is a bit like Show.
data instance Sing (a :: PathComponentKind) = SingPathComponent String

-- | Sing-introduction instance for constant path components.
instance (SingI res) => SingI (S res) where
  sing = SingPathComponent $ fromSing (sing :: Sing res)

-- | Sing-introduction instance for variable path components.
instance (SingI res, Typeable a) => SingI (A res a) where
  sing = SingPathComponent $ "{" ++ (fromSing (sing :: Sing res)) ++ " :: " ++ (show $ typeOf (undefined :: a)) ++ "}"

-- | Sing-elimination, that simply unwraps tha data instance to a String.  Note
-- by the way the presence of a Show instance with context '(SingE (Kind :: k)
-- rep, Show rep)'.
instance SingE (Kind :: PathComponentKind) String where
  fromSing (SingPathComponent component) = component

-- | We create a Sing (short for singleton) instance to hook into the
-- GHC.TypeLits mechanism for going from type level to value level.  This is
-- facilitated by the typeclasses SingI and SingE.
data instance Sing (a :: HttpPathKind) = SingHttpPath [String]

-- Sing-introduction for Nil.
instance SingI Nil where
  sing = SingHttpPath []

-- Sing-introduction for (:/:).
instance (SingI httpPath, SingI component) =>
  SingI ((component :: PathComponentKind) :/: (httpPath :: HttpPathKind)) where
  sing = let SingHttpPath rest = sing :: Sing httpPath
             top = fromSing (sing :: Sing component) :: String
         in SingHttpPath $ top : rest

-- | Sing-elimination, that simply unwraps tha data instance to a String.
instance SingE (Kind :: HttpPathKind) String where
  fromSing (SingHttpPath lst) = Prelude.foldl (\x y -> x ++ "/" ++ y) "" lst

-- | To properly serialise HttpPathFn path arguments we need a dedicated
-- typeclass, as opposed to just Show or ToJson. Instances of this class should
-- take care to obey the restrictions of resource paths in Http.
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
