STDOUT=stdout.log

./test > $STDOUT

# The usage text is split across multiple lines making the greps unreliable.
# This joins together the output onto one line, with correct spaceage in the
# usage text so that the greps can find the expected text.
python parse_output.py

# Check that the help text contains updated usage information
if grep -P -q "\[--charset\|-C <charset name>\]" $STDOUT; then
   echo "stdout contained new usage text for option"
fi

if grep -P -q "\[--day\|-D <three letter day of week>\]" $STDOUT; then
   echo "stdout contained new usage text for enum option"
fi

if grep -P -q "\[--files\|-F <list of filepaths to parse>\]" $STDOUT; then
   echo "stdout contained new usage text for option list"
fi

# Check that the help text contains default usage information
if grep -P -q "\[--charset2\|-C2 CHARSET2\]" $STDOUT; then
   echo "stdout contained default usage text for option"
fi

if grep -P -q "\[--day2\|-D2 DAY2\]" $STDOUT; then
   echo "stdout contained default usage text for enum option"
fi

if grep -P -q "\[--files2\|-F2 FILES2 \[FILES2...\]\]" $STDOUT; then
   echo "stdout contained default usage text for option list"
fi
