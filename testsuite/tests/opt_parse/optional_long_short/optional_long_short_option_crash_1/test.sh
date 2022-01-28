STDOUT=stdout.log

./test > $STDOUT 2>&1

# Check that the help text contains expected exception
if grep -q "raised GNATCOLL.OPT_PARSE.OPT_PARSE_ERROR" $STDOUT; then
   echo "stdout contained expected exception"
fi