module Test where

import Imprevu.Test
import Distribution.TestSuite

tests :: IO [Test]
tests = return [
   test testSingleInputEx    "single input",
   test testMultipleInputsEx "multiple inputs",
   test testInputStringEx    "input string",
   test testSendMessageEx    "send message",
   test testSendMessageEx2   "send message 2",
   test testAPICallEx        "API Call",
   test testAPICallEx2       "API Call 2",
   test testUserInputWriteEx "User input write",
   test testSumComposeEx     "sum compose",
   test testProdComposeEx1   "prod compose",
   test testProdComposeEx2   "prod compose 2",
   test testTwoEventsEx      "two events",
   test testMonadicEventEx   "monadic event",
   test testShorcutEventEx   "shortcut event",
   test testDoubleEventEx    "double event",
   test testTimeEventEx      "time event",
   test testTimeEventEx2     "time event 2"
   ]

test :: Bool -> String -> Test
test p t = Test $ test' p t

test' :: Bool -> String -> TestInstance
test' p t = TestInstance
   { run = return $ Finished $ if p then Pass else Fail ""
   , name = t
   , tags = []
   , options = []
   , setOption = \_ _ -> Right $ test' p t
   }

