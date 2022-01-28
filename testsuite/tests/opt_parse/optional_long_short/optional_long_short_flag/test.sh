STDOUT=stdout.log

./test -F1 -F2 --flag3 > $STDOUT
./test --help >> $STDOUT

# Check that the input's are correctly parsed
if grep -q "Flag 1 TRUE" $STDOUT; then
   echo "stdout contained expected output for FLAG1"
fi
if grep -q "Flag 2 TRUE" $STDOUT; then
   echo "stdout contained expected output for FLAG2"
fi
if grep -q "Flag 3 TRUE" $STDOUT; then
   echo "stdout contained expected output for FLAG3"
fi

# Check that output does not contain an OPT_PARSE_ERROR
if grep -q "raised GNATCOLL.OPT_PARSE.OPT_PARSE_ERROR" $STDOUT; then
   echo "stdout contained unexpected exception"
fi
