-----------------------------------------------------------------------------
--
-- Module      :  Help
-- Copyright   :
-- License     :  OtherLicense
--
-- Maintainer  :  corentin.dupont@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Web.Help where
upload = "You can upload your file containing custom rules here. File must be a haskell .hs file containing a module. Once correctly loaded, you will be able to propose the functions in the file as new rules."
code = "This is where you type in your new rule. Its type must be \"RuleFunc\". As a first rule, you can try to type \"nothing\", which is a rule that does nothing. Other examples can be found in the file Examples.hs accessible on the left tab."
actives = "Those rules are active and control the game. Usually, initial rules define how to propose and activate new rules (for example, a democratic vote) and how to win. Of course, this can be changed!"
pendings = "Rules are pending after being proposed by a player. Only another, active rule can activate those rules with instruction \"activateRule\"."
inputs = "Rules can trigger an input to gather data from the player, for example with the instruction \"onInputChoice\"."
events = "Rules can register on events, in order to be triggered when the event happens, for example with the instruction \"onEvent\"."
variables = "Rules can create variables to store data. For example, a rule creating a bank account with instruction \"newVar\" will make a new variable appear here."
outputs = "Rules can display text on players screen, with instruction \"output\"."
