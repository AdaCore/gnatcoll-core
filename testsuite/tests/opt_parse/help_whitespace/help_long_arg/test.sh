STDOUT=stdout.log

./test --help > $STDOUT

# Check that the help message is structured as expected
if grep -q -P "( ){20,}" $STDOUT; then
    # Opposite to help_short_80. There should be more than 20 spaces, which
    # was the previous limit after a single character arg like "-c" and a
    # fixed 25 character column
    echo "Help message column increased as expected"
fi
