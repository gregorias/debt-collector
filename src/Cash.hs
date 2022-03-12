module Cash (
  Cash (..),
  cashP,
  negate,
) where

import Currency (Currency, currencyP)
import Data.Decimal (Decimal)
import MyDecimal (decimalP, defaultDecimalFormat)
import Relude
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

data Cash = Cash
  { _currency :: !Currency
  , _amount :: !Decimal
  }
  deriving stock (Eq, Ord, Show)

cashP ::
  ( MonadFail m
  , MP.MonadParsec e s m
  , MP.Token s ~ Char
  ) =>
  m Cash
cashP = currencyFirstP <|> currencySecondP
 where
  currencyFirstP = do
    currency <- currencyP
    MP.space1
    amount <- decimalP defaultDecimalFormat
    return $ Cash currency amount
  currencySecondP = do
    amount <- decimalP defaultDecimalFormat
    MP.space1
    currency <- currencyP
    return $ Cash currency amount
