module Test.Network.HTTP.Rest.Match where

import Network.HTTP.Rest.Match

matched :: Eq a => a -> Match e a -> Bool
matched x (MatchSuccess _ x') | x == x' = True
matched _ _ = False
