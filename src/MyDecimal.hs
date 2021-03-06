-- | My extended parsing utilities for parsing decimals.
module MyDecimal (
  decimalP,
  DecimalFormat (..),
  ChunkSepFormat (..),
  DecimalFractionFormat (..),
  defaultDecimalFormat,
  cashDecimalFormat,
) where

import Data.Char (digitToInt)
import Data.Decimal (Decimal)
import Data.Ratio ((%))
import Relude
import Text.Megaparsec (
  MonadParsec,
  Token,
 )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, digitChar, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

-- | 'DecimalFractionFormat' describes the format of the fraction part of a
-- decimal.
data DecimalFractionFormat
  = TwoDigitDecimalFraction
  | OptionalUnlimitedDecimalFraction

data ChunkSepFormat = NoChunkSep | ChunkSep Char

chunkSepFormat :: a -> (Char -> a) -> ChunkSepFormat -> a
chunkSepFormat def _ NoChunkSep = def
chunkSepFormat _ f (ChunkSep c) = f c

-- | 'DecimalFormat' describes the format of a decimal.
-- If a fraction part is present, then it starts with `.`
data DecimalFormat = DecimalFormat
  { -- | The optional character used to separate 3 digit chunks.
    decimalFormatChunkSep :: !ChunkSepFormat
  , -- | 'decimalFormatFractionFormat' describes the format of an optional
    -- fraction part of a decimal.
    decimalFormatFractionFormat :: !(Maybe DecimalFractionFormat)
  }

-- | The default format that has no chunk separation and can have a fractional
-- part of arbitrary length.
defaultDecimalFormat :: DecimalFormat
defaultDecimalFormat = DecimalFormat NoChunkSep (Just OptionalUnlimitedDecimalFraction)

-- | The format that has no chunk separation and has a two-digit
-- fractional part of arbitrary length.
cashDecimalFormat :: ChunkSepFormat -> DecimalFormat
cashDecimalFormat = flip DecimalFormat (Just TwoDigitDecimalFraction)

-- | A decimal parser that handles separators and fractions.
--
-- >>> parseMaybe (decimalP defaultDecimalFormat) "-2000.41"
-- Just -2000.41
decimalP ::
  ( MonadFail m
  , MonadParsec e s m
  , Token s ~ Char
  ) =>
  DecimalFormat ->
  m Decimal
decimalP (DecimalFormat chunkSep fractionFormat) = signed space $ do
  units :: Integer <- chunkSepFormat decimal decimalWithSepP chunkSep
  fract <- case fractionFormat of
    Nothing -> pure 0
    Just format -> decimalFractionP format
  return $ fromRational (units % 1 + fract)

decimalWithSepP ::
  ( MonadFail m
  , MonadParsec e s m
  , Token s ~ Char
  ) =>
  Char ->
  m Integer
decimalWithSepP sep = do
  wholeString <- some (digitChar <|> char sep)
  let digits = fmap (toInteger . digitToInt) . filter (/= sep) $ wholeString
  return $ foldl' (\a d -> a * 10 + d) 0 digits

decimalFractionP ::
  ( MonadParsec e s m
  , Token s ~ Char
  ) =>
  DecimalFractionFormat ->
  m Rational
decimalFractionP TwoDigitDecimalFraction = do
  void $ char '.'
  fractionalString <- replicateM 2 digitChar
  return $ fractionStringToRational fractionalString
decimalFractionP OptionalUnlimitedDecimalFraction =
  ( do
      void $ char '.'
      fractionalString <- MP.some digitChar
      return $ fractionStringToRational fractionalString
  )
    <|> pure 0

fractionStringToRational :: [Char] -> Rational
fractionStringToRational =
  uncurry (%)
    . foldl' (\(v, b) d -> (v + d * b, b * 10)) (0, 1)
    . dropWhile (== 0)
    . fmap (toInteger . digitToInt)
    . reverse
