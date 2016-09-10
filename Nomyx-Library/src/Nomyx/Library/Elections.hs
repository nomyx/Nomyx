
-- | This file gives a list of example rules that the players can submit.
module Nomyx.Library.Elections where

import Data.List
import Control.Monad
import Language.Nomyx


tournamentMasterCandidates :: Rule
tournamentMasterCandidates = do
   let tournamentMasterCandidates = V "tournamentMasterCandidates" :: V [PlayerNumber]
   let candidate pn = void $ modifyVar tournamentMasterCandidates (pn : )
   let displayCandidates pns = return $ "Candidates for the election of Tournament Master: Players #" ++ intercalate ", " (map show pns)
   newVar_ (varName tournamentMasterCandidates) ([] :: [PlayerNumber])
   forEachPlayer_ (\pn -> void $ onInputButtonOnce "I am candidate for the next Tournament Master elections " (const $ candidate pn) pn)
   void $ displayVar' Nothing tournamentMasterCandidates displayCandidates

