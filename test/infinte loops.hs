
-- works OK
ruleFunc $ do
   let x = 1 + x
   outputAll_ $ show x

--makes memory & CPU explode
ruleFunc $ outputAll_ $ show $ repeat 1


