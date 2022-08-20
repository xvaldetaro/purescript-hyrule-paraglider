module Paraglider.Operator.StartingWith
  ( startingWith
  )
  where

import Prelude

import Control.Alt (class Alt, (<|>))

startingWith :: forall e a. Alt e => Applicative e => a -> e a -> e a
startingWith a e = pure a <|> e