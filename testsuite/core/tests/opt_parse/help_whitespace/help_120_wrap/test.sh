STDOUT=stdout.log

./test --help > $STDOUT

# Check that the help message is structured as expected
if grep -q -P ".{81,}" $STDOUT; then
    # Confirm that when permitted, the help text can exceed the previous 80
    # character limit without wrapping.
    echo "Help text doesn't wrap at 80 characters when permitted, as expected"
fi
