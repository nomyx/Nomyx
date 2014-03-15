#Regular tests
Nomyx -t
TestsRet=$?

#This tests should make Nomyx die hard"
Nomyx -l "Timeout type check"
KillTest=$?

if [ $TestsRet -eq 0 ] || [ $KillTest -ne 0 ]; then
   echo Nomyx tests: Success
else
   echo Nomyx tests: Failure
fi
