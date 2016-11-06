#Regular tests
stack exec Nomyx -- -t
TestsRet=$?

#This tests should make Nomyx die hard"
stack exec Nomyx -- -t -l "Timeout type check"
KillTest=$?

if [ $TestsRet -eq 0 ]; then
   echo Nomyx tests: Success
   true
else
   echo Nomyx tests: Failure
   false
fi
