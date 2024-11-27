STDOUT=stdout.log

./test --help > $STDOUT

# Check that the help message is structured as expected
if ! grep -q -P "( ){4,}" $STDOUT; then
    # The biggest whitespace should be the three characters before optional
    # arguments as "--help, -h" and "--char, -C" are the same lenght, and
    # while they are less than 25 characters, the column number the help
    # text starts at should be limited to 2 after the longest arg text.
    echo "Help message column reduced as expected"
fi
