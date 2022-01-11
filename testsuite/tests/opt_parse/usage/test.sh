STDOUT=stdout.log

./test > $STDOUT

# Check that the help text contains updated usage information
if grep -q "[--charset|-C <charset name>]" $STDOUT; then
   echo "stdout contained new usage text for option"
else
   echo "stdout missing new usage text for option"
fi

if grep -q "[--day|-D <three letter day of week>]" $STDOUT; then
   echo "stdout contained new usage text for enum option"
else
   echo "stdout missing new usage text for enum option"
fi

if grep -q "[--files|-F <list of filepaths to parse>]" $STDOUT; then
   echo "stdout contained new usage text for option list"
else
   echo "stdout missing new usage text for option list"
fi

# Check that the help text contains default usage information
if grep -q "[--charset2|-C2 CHARSET2]" $STDOUT; then
   echo "stdout contained default usage text for option"
else
   echo "stdout missing default usage text for option"
fi

if grep -q "[--day2|-D2 DAY2]" $STDOUT; then
   echo "stdout contained default usage text for enum option"
else
   echo "stdout missing default usage text for enum option"
fi

if grep -q "[--files2|-F2 FILES2[FILES2...]]" $STDOUT; then
   echo "stdout contained default usage text for option list"
else
   echo "stdout missing default usage text for option list"
fi
