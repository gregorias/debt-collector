-- | This module extends 'Alpha'.
module Currency (
  Currency (..),
  currencyP,
  chf,
  eur,
  pln,
  usd,
) where

import Control.Applicative.Combinators (count)
import Data.Currency (Alpha (CHF, EUR, PLN, USD))
import Relude
import Text.Megaparsec (MonadParsec)
import Text.Megaparsec.Char (letterChar)
import Text.Megaparsec.Stream (Stream (Token))

-- | 'Currency' wraps 'Alpha' to provide additional instances and custom parsers.
newtype Currency = Currency Alpha
  deriving newtype (Bounded, Eq, Enum, Ord, Read, Show, NFData)

parseCurrency :: Text -> Maybe Currency
parseCurrency = inverseMap show

currencyP ::
  ( MonadFail m
  , MonadParsec e s m
  , Token s ~ Char
  ) =>
  m Currency
currencyP = do
  Just cur <- parseCurrency . toText <$> count 3 letterChar
  return cur

chf :: Currency
chf = Currency CHF

eur :: Currency
eur = Currency EUR

pln :: Currency
pln = Currency PLN

usd :: Currency
usd = Currency USD
