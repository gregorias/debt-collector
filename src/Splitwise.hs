module Splitwise (
  createSplitwiseExpense,
  ApiKey,
) where

import Cash (Cash (..))
import Control.Monad.Except (
  MonadError (..),
  throwError,
 )
import Data.Decimal (Decimal)
import qualified Data.Decimal as D
import Relude
import Turtle (ExitCode (ExitFailure, ExitSuccess), procStrictWithErr)

-- | Shows a Decimal in Splitwise acceptable format.
showDecimal :: Decimal -> Text
showDecimal = show . D.roundTo 2

-- | Splitwise API Key
-- https://dev.splitwise.com/#section/Authentication/ApiKeyAuth
type ApiKey = Text

type Debtor = Text

data ExpenseUser = ExpenseUser
  { _id :: !Text
  , _paidShare :: !Decimal
  , _owedShare :: !Decimal
  }

showExpenseUser :: ExpenseUser -> Text
showExpenseUser (ExpenseUser userId paidShare owedShare) =
  userId <> "," <> showDecimal paidShare <> "," <> showDecimal owedShare

splitwiseCli ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  ApiKey ->
  Cash ->
  [ExpenseUser] ->
  m ()
splitwiseCli apiKey (Cash currency amount) expenseUsers = do
  (exitCode, _out, err) <-
    liftIO $
      procStrictWithErr
        "/Users/grzesiek/.local/bin/splitwise-cli"
        ( [ "--api_key=" <> apiKey
          , "--description=automated entry"
          , "--cost=" <> showDecimal amount
          , "--currency_code=" <> show currency
          ]
            <> (showExpenseUser <$> expenseUsers)
        )
        mempty
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure _ -> throwError $ "Splitwise-cli has failed.\n" <> err

grzegorzId :: Text
grzegorzId = "2944437"

janId :: Text
janId = "695637"

createSplitwiseExpense :: (MonadIO io) => ApiKey -> Cash -> Debtor -> ExceptT Text io ()
createSplitwiseExpense apiKey cost debtor = do
  unless (debtor == "Jan Kwaśniak") (throwError "Currently handling only Janek as a debtor.")
  splitwiseCli
    apiKey
    cost
    [ ExpenseUser{_id = grzegorzId, _paidShare = _amount cost, _owedShare = 0}
    , ExpenseUser{_id = janId, _paidShare = 0, _owedShare = _amount cost}
    ]
