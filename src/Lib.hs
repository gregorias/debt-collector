module Lib (
  main,
) where

import Cash (
  Cash (..),
  cashP,
 )
import Control.Monad.Cont (
  MonadCont (..),
  runContT,
 )
import Control.Monad.Writer (
  MonadWriter (tell),
  WriterT (runWriterT),
 )
import qualified Data.Text as T
import Data.Text.IO (hPutStrLn)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Relude
import Splitwise (createSplitwiseExpense)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import Text.Regex (
  mkRegex,
  subRegex,
 )
import qualified Turtle

data PendingDebt = PendingDebt
  { _debtor :: !Text
  , _cost :: !Cash
  }

type Parser = MP.Parsec Void Text

pendingDebtP :: Parser PendingDebt
pendingDebtP = do
  void . MP.try $ do
    MP.space1 >> MP.char '!' >> MP.space1
    MP.string "Assets:Debts:"
  debtor <- toText <$> MP.manyTill MP.anySingle doubleSpaceOrEol
  void MP.space
  cost <- cashP
  void MP.takeRest
  return $ PendingDebt debtor cost
 where
  doubleSpaceOrEol = void (MP.string "  ") <|> void MP.eol <|> MP.eof

-- | Collects debt if the line represents a pending debt.
checkAndCollectDebt ::
  (MonadIO m, MonadWriter Text m, MonadCont m) =>
  Turtle.Line ->
  m Turtle.Line
checkAndCollectDebt line = callCC $ \exit -> do
  let maybePendingDebt = parseLine line
  pendingDebt <- maybe (exit line) return maybePendingDebt
  serviceDebt pendingDebt
 where
  parseLine :: Turtle.Line -> Maybe PendingDebt
  parseLine = MP.parseMaybe pendingDebtP . Turtle.lineToText

  serviceDebt :: (MonadIO m, MonadWriter Text m, MonadCont m) => PendingDebt -> m Turtle.Line
  serviceDebt (PendingDebt debtor cost) = callCC $ \exit -> do
    maybeApiKey <-
      readMaybe
        . toString
        . T.strip
        . decodeUtf8
        <$> readFileBS "/Users/grzesiek/Code/findata/debt-collector/splitwise-api-key.txt"
    apiKey <-
      maybe
        ( do
            tell "Could not read the API key.\n"
            exit line
        )
        return
        maybeApiKey
    result <- runExceptT $ createSplitwiseExpense apiKey cost debtor
    case result of
      Left errMsg -> do
        tell $ "Could not create a splitwise expense for " <> debtor <> ".\n" <> errMsg
        return line
      Right () -> return $ replacePendingWithCleared line

  replacePendingWithCleared :: Turtle.Line -> Turtle.Line
  replacePendingWithCleared debtorLine =
    let lineTxt = Turtle.lineToText debtorLine
        regex = mkRegex "^( *)!"
     in Turtle.unsafeTextToLine . toText $ subRegex regex (toString lineTxt) "\\1*"

main :: IO ()
main = do
  -- My wallet and most of my files are in UTF-8, so make sure it's used.
  setLocaleEncoding utf8
  Turtle.update checkAndCollectDebtSh "/Users/grzesiek/wallet/wallet.txt"
 where
  checkAndCollectDebtSh :: Turtle.Shell Turtle.Line -> Turtle.Shell Turtle.Line
  checkAndCollectDebtSh shellLines = do
    line <- shellLines
    (newLine, errMsgs) <- runContT (runWriterT (checkAndCollectDebt line)) return
    unless (T.null errMsgs) $ liftIO $ hPutStrLn stderr $ "Could not collect debt for line " <> Turtle.lineToText line <> ".\n" <> errMsgs
    return newLine
