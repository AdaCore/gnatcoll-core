STDOUT=stdout.log

INPUT1="file1 file11 file111"
INPUT2="file2 file22 file222"
INPUT3="file3 file33 file333"

./test -F1 $INPUT1 -F2 $INPUT2 --files3 $INPUT3 > $STDOUT
./test --help >> $STDOUT

# Check that the input's are correctly parsed
if grep -q "$INPUT1" $STDOUT; then
   echo "stdout contained expected output for INPUT1"
fi
if grep -q "$INPUT2" $STDOUT; then
   echo "stdout contained expected output for INPUT2"
fi
if grep -q "$INPUT3" $STDOUT; then
   echo "stdout contained expected output for INPUT3"
fi

# Check that output does not contain an OPT_PARSE_ERROR
if grep -q "raised GNATCOLL.OPT_PARSE.OPT_PARSE_ERROR" $STDOUT; then
   echo "stdout contained unexpected exception"
fi


# Check that output does not contain a CONSTRAINT_ERROR
if grep -q "raised CONSTRAINT_ERROR" $STDOUT; then
   echo "stdout contained unexpected exception"
fi