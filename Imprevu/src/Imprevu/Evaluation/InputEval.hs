
-- | Evaluation of the inputs
module Imprevu.Evaluation.InputEval where

import           Imprevu.Types
import           Imprevu.Evaluation.Types
import           Imprevu.Evaluation.EventEval

triggerInput :: Input -> InputData -> EvaluateN n s ()
triggerInput is@(Input (Text _) _)       (TextData a)     = triggerEvent (Signal is) a
triggerInput is@(Input (TextArea _) _)   (TextAreaData a) = triggerEvent (Signal is) a
triggerInput is@(Input (Button _) _)     (ButtonData)     = triggerEvent (Signal is) ()
triggerInput is@(Input (Radio _ _) _)    (RadioData a)    = triggerEvent (Signal is) a
triggerInput is@(Input (Checkbox _ _) _) (CheckboxData a) = triggerEvent (Signal is) a
triggerInput _ _ = error "triggerInput"
