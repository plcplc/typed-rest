-- | This module defines an 'Applicative' and 'Monad' for inspecting
-- pattern matching, motivated by the desire to give good error messages
-- when validating input.
module Network.HTTP.Rest.Match where

import Control.Applicative
import Data.Monoid

-- | A type representing an expectation, typically of a match.
data Expected pat a = Expected {
  emExpected  :: pat,
  emActual    :: a,
  emSatisfied :: Bool
  } deriving (Eq, Show)

expected :: pat -> a -> Expected pat a
expected p x = Expected p x True

unExpected :: pat -> a -> Expected pat a
unExpected p x = Expected p x False

instance (Monoid a, Monoid p) => Monoid (Expected p a) where
  mempty = Expected mempty mempty False
  mappend (Expected x1 x2 sat1) (Expected y1 y2 sat2)
    = Expected (mappend x1 y1) (mappend x2 y2) (sat1 && sat2)

-- | Type representing a series of pattern matches, both successful and
-- failing. The 'Applicative' and 'Monad' instances record each match using
-- 'mappend' for the match info 'e'.
data Match e a = MatchErr e | MatchSuccess e a
  deriving (Eq, Show)

instance Monoid e => Functor (Match e) where

  fmap _ (MatchErr e) = MatchErr e
  fmap f (MatchSuccess e x) = MatchSuccess e $ f x

instance Monoid e => Applicative (Match e) where

  pure = MatchSuccess mempty

  (MatchErr e1)    <*> (MatchErr e2)    = MatchErr $ e1 <> e2
  (MatchErr e1)    <*> (MatchSuccess e2 _) = MatchErr (e1 <> e2)
  (MatchSuccess e1 _) <*> (MatchErr e2)     = MatchErr (e1 <> e2)
  (MatchSuccess e1 f) <*> (MatchSuccess e2 x) = MatchSuccess ( e1 <> e2) (f x)

instance (Monoid e) => Monad (Match e) where

  return = MatchSuccess mempty

  (MatchSuccess e x) >>= f =
    case f x of
      MatchErr e'        -> MatchErr (e <> e')
      MatchSuccess e' x' -> MatchSuccess (e <> e') x'

  (MatchErr e)     >>= _ = MatchErr e

match :: Monoid e => (Bool -> e) -> Maybe a -> Match e a
match e (Just x) = MatchSuccess (e True) x
match e Nothing = MatchErr (e False)

matchE :: Monoid e => Either e a -> Match e a
matchE (Right x) = MatchSuccess mempty x
matchE (Left e)  = MatchErr e

matchFail :: Monoid e => (Bool -> e) -> Match e a
matchFail = MatchErr . ($ False)

matchCase :: Monoid e => [Match e a] -> Match e a
matchCase [] = matchFail mempty
matchCase ((MatchErr e):ms) = mapMatch (e <>) $ matchCase ms
matchCase (m:_) = m

isMatchSuccess :: Match e a -> Bool
isMatchSuccess (MatchSuccess _ _) = True
isMatchSuccess _ = False

isMatchFailed :: Match e a -> Bool
isMatchFailed (MatchErr _) = True
isMatchFailed _ = False

guard' :: Monoid e => (Bool -> e) -> Bool -> Match e ()
guard' e True  = MatchSuccess (e True) ()
guard' e False = MatchErr (e False)

mapMatch :: (Monoid e, Monoid e') => (e -> e') -> Match e a -> Match e' a
mapMatch f (MatchErr e)     = MatchErr $ f e
mapMatch f (MatchSuccess e x) = MatchSuccess (f e) x
