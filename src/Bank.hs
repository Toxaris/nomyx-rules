{-# LANGUAGE DeriveDataTypeable #-}
module Bank where

import Prelude
import Language.Nomyx
import Data.Typeable

-- Copied from the Rules.hs used in the Nomyx live instances.
getValueOfPlayer :: PlayerNumber -> V [(Int, Int)] -> Nomex (Maybe Int)
getValueOfPlayer pn var = do
   value <- readVar_ var
   return $ lookup pn value

-- Input a Read-able value.
onInput_ :: Read a => String -> (a -> Nomex ()) -> PlayerNumber -> Nomex ()
onInput_ title handler player = onInputStringOnce_ title safeHandler player where
  safeHandler text = case reads text of
    [(result, "")] -> handler result
    _ -> onInput_ title handler player

-- A datatype for selecting players.
newtype PrettyPlayer = PrettyPlayer PlayerInfo
  deriving (Typeable, Eq)

instance Show PrettyPlayer where
  show (PrettyPlayer p) = _playerName p ++ "(Player #" ++ show (_playerNumber p) ++ ")"

-- Create a place that players can go and do actions.
createPlace :: (Bounded action, Enum action, Eq action, Show action, Typeable action) =>
  String -> (action -> PlayerNumber -> Nomex ()) -> Nomex ()
createPlace name handler = forEachPlayer_ selectAction where
  selectAction player = onInputChoiceEnum_ message (toEnum 0) (\action -> handler action player) player
  message = "Go to " ++ name ++ " and "

-- The actions you can do at the bank.
data BankAction
  = TransferMoney
  | AskForYourBalance
  deriving (Enum, Bounded, Typeable, Eq)

instance Show BankAction where
  show TransferMoney = "transfer money"
  show AskForYourBalance = "ask for your balance"

-- Create a bank.
createBank :: RuleFunc
createBank = voidRule $ createPlace "the bank" handleBankAction

-- Execute an action you can do at the bank.
handleBankAction :: BankAction -> PlayerNumber -> Nomex ()
handleBankAction TransferMoney src = do
  players <- getPlayers
  let
    inputDst = onInputChoice_
      "Transfer money to whom? "
      [PrettyPlayer p | p <- players, _playerNumber p /= src]
      inputAmount
      src
    inputAmount dstInfo = onInput_
      ("Transfer how much money to " ++ show dstInfo ++ "? ")
      (execute dstInfo)
      src
    execute dstInfo amount = do
      Just balance <- getValueOfPlayer src accounts
      let PrettyPlayer (PlayerInfo {_playerNumber = dst}) = dstInfo
      Just srcInfo' <- getPlayer src
      let srcInfo = PrettyPlayer srcInfo'
      when ((amount >= 0) && (balance >= amount)) $ do
        modifyValueOfPlayer dst accounts (+ amount)
        modifyValueOfPlayer src accounts (subtract amount)
        output ("You gave " ++ show amount ++ " to " ++ show dstInfo ++ ".") src
        output (show srcInfo ++ " gave you " ++ show amount ++ ".") dst
  inputDst
handleBankAction AskForYourBalance src = do
  Just balance <- getValueOfPlayer src accounts
  when (balance > 1) $ do
    modifyValueOfPlayer src accounts (subtract 1)
    output "Our fee for asking is 1." src

  Just balance <- getValueOfPlayer src accounts
  output ("Your account balance is " ++ show balance ++ ".") src
