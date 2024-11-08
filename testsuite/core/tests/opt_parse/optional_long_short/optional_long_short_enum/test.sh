STDOUT=stdout.log

INPUT1=MON
INPUT2=TUE
INPUT3=WED

./test -D1 $INPUT1 -D2 $INPUT2 --day3 $INPUT3 > $STDOUT
./test --help >> $STDOUT

# Check that the input's are correctly parsed
if grep -q $INPUT1 $STDOUT; then
   echo "stdout contained expected output for INPUT1"
fi
if grep -q $INPUT2 $STDOUT; then
   echo "stdout contained expected output for INPUT2"
fi
if grep -q $INPUT3 $STDOUT; then
   echo "stdout contained expected output for INPUT3"
fi

# Check that output does not contain an OPT_PARSE_ERROR
if grep -q "raised GNATCOLL.OPT_PARSE.OPT_PARSE_ERROR" $STDOUT; then
   echo "stdout contained unexpected exception"
fi
