STDOUT=stdout.log

./test --help >> $STDOUT
./test sub1 --help >> $STDOUT
./test sub2 --help >> $STDOUT

cat $STDOUT
echo "" $(pwd)

# # Check that the input's are correctly parsed
# if grep -q $INPUT1 $STDOUT; then
#    echo "stdout contained expected output for INPUT1"
# fi
# if grep -q $INPUT2 $STDOUT; then
#    echo "stdout contained expected output for INPUT2"
# fi
# if grep -q $INPUT3 $STDOUT; then
#    echo "stdout contained expected output for INPUT3"
# fi

# # Check that output does not contain an OPT_PARSE_ERROR
# if grep -q "raised GNATCOLL.OPT_PARSE.OPT_PARSE_ERROR" $STDOUT; then
#    echo "stdout contained unexpected exception"
# fi
