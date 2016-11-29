
-- | Evaluation of the inputs
module Imprevu.Evaluation.InputEval where

import           Imprevu.Types
import           Imprevu.Evaluation.Types
import           Imprevu.Evaluation.EventEval

triggerInput :: InputS -> InputData -> EvaluateN n s ()
triggerInput is@(InputS (Text _) _)       (TextData a)     = triggerEvent (Signal is) a
triggerInput is@(InputS (TextArea _) _)   (TextAreaData a) = triggerEvent (Signal is) a
triggerInput is@(InputS (Button _) _)     (ButtonData)     = triggerEvent (Signal is) ()
triggerInput is@(InputS (Radio _ _) _)    (RadioData a)    = triggerEvent (Signal is) a
triggerInput is@(InputS (Checkbox _ _) _) (CheckboxData a) = triggerEvent (Signal is) a
triggerInput _ _ = error "triggerInput"
