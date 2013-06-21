{-# LANGUAGE DoAndIfThenElse, GADTs #-}
module Withdraw where

import Prelude
import Language.Nomyx

-- A message when the pending rules of a player change
pendingChanged :: Msg PlayerNumber
pendingChanged = Message "pendingChanged"

-- Ask a player about withdrawing pending rules.
askWithdrawRule :: PlayerNumber -> Nomex ()
askWithdrawRule pn = do
  -- ask the question (if necessary)
  rules <- getRules
  let pending = [ _rNumber rule
                | rule <- rules
                , _rStatus rule == Pending
                , _rProposedBy rule == pn]
  question <-
    if not (null pending)
    then onInputChoice
           "Withdraw pending rule: "
           pending
           (\en rn -> rejectRule_ rn)
           pn
    else return (-1)

  -- maintain the question
  onEvent pendingChanged $
    \(self, MessageData pn') ->
      when (pn' == pn) $ do
        delEvent_ self
        delEvent_ question
        askWithdrawRule pn

  return ()

-- enable rule withdrawing
enableRuleWithdrawing = voidRule $ do
  let handler (RuleData rule) =
        sendMessage pendingChanged (_rProposedBy rule)
  onEvent_ (RuleEv Proposed)  handler
  onEvent_ (RuleEv Activated) handler
  onEvent_ (RuleEv Rejected)  handler
  forEachPlayer_ askWithdrawRule
