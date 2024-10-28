STDOUT=stdout.log

./test --help > $STDOUT

# Check that the help message is structured as expected
if ! grep -q -P ".{81,}" $STDOUT; then
    # Confirm that help text wraps at the 80 character limit as expected
    echo "Help text wrapped at 80 characters as expected"
fi
