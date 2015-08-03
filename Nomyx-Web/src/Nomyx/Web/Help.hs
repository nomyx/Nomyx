
module Nomyx.Web.Help where

upload, rules, inputsOutputs, code, events, variables, view, joinGame, fork, getSaveFile :: String
upload = "In the following form you can upload your file containing custom rules. The file must be a haskell .hs file containing a module, \n" ++
         "like in the given example. Once correctly loaded, the content of the file will be \"in scope\" and you will be able to propose the functions \n" ++
         "contained in the file as new rules (in this example, you will be able to propose the rule \"myRule\").\n" ++
         "Additionally, your file will appear as a link on the left hand side on the main page.\n" ++
         "Warning, files uploaded here cannot be overwritten. If your uploaded file contains a mistake,\n" ++
         "re-upload it with a different name (a version number suffixed for example) or ask the administrator to delete it.\n"

rules = "The rules are displayed here. The active rules are controlling the game. " ++
        "When a player proposes a rule, it is set to pending. Only another, already active rule can activate a pending rule (with the instruction \"activateRule\")."
inputsOutputs = "The inputs and outputs triggered by the rules are displayed here.\n For example, a rules can trigger an input to gather data from the player, with the instruction \"onInputRadio\".\n" ++
                "Rules can also display text message here, with the instruction \"output\"."

code = "You can type in your new rule in the box below. The text entered must have the type \"Rule\" when compiled.\n" ++
       "When you are done, you can press \"Check\" to verify if the rule compiles.\n" ++
       "If OK, press \"Submit\" to propose the new rule.\n" ++
       "As a first rule, you can try to type \"nothing\", which is a rule that does nothing. Other examples can be found in the examples link on the left tab."
events = "Rules can register on events, in order to be triggered when the event happens, for example with the instruction \"onEvent\".\n"
variables = "Variables: Rules can create variables to store data. For example, a rule creating a bank account with instruction \"newVar\" will make a new variable appear here."

view = "Only view a game. You will not be able to propose new rules."
joinGame = "Be part of the game. You will be able to propose new rules, vote etc. Please register in the game's forum (see the link on game page) to follow the game."

fork = "This will create a new game based on the previous one. You will be able to test \n" ++
        "your new rules independently of the original game. The new game is completely private: you will be alone. Please delete it when finished."


getSaveFile = "With the following link, you can download the save file of the game. This allows you to load it in a local instance of the game.\n" ++
              "This way, you will be able to compose and test the effects of your new rules locally, without affecting the online game. \n" ++
              "The procedure is: \n" ++
              "$> cabal install Nomyx \n" ++
              "$> Nomyx -r <save file name>\n" ++
              "Warning: Nomyx should have the exact same version as the online instance.\n"
