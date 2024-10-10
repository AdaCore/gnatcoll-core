******************************
**Templates**: generating text
******************************

.. index:: templates

This module provides convenient subprograms for replacing specific
substrings with other values. It is typically used to replace substrings
like "%{version}" in a longer string with the actual version, at run time.

This module is not the same as the templates parser provided in the context
of AWS, the Ada web server, where external files are parsed and processed
to generate other files. The latter provides advanced features like filters,
loops,...

The substrings to be replaced always start with a specific delimiter, which
is set to `%` by default, but can be overridden in your code. The name
of the substring to be replaced is then the identifier following that
delimiter, with the following rules:

* If the character following the delimiter is the delimiter itself,
  then the final string will contain a single instance of that delimiter, and
  no further substitution is done for that delimiter. An example of this is
  `"%%"`.

* If the character immediately after the delimiter is a curly brace
  (`{`), then the name of the identifier is the text until the next
  closing curly brace. It can then contain any character expect a closing
  curly brace. An example of this is `"%{long name}"`

* If the first character after the delimiter is a digit, then the
  name of the identifier is the number after the delimiter. An example of
  this is `"%12"`. As a special case, if the first non-digit
  character is the symbol `-`, it is added as part of the name of the
  identifier, as in `"%1-"`. One use for this feature is to indicate
  you want to replace it with all the positional parameters %1%2%3%4. For
  instance, if you are writing the command line to spawn an external tool,
  to which the user can pass any number of parameter, you could specify that
  command line as `"tool -o %1 %2-"` to indicate that all parameters
  should be concatenated on the command line.

* If the first character after the delimiter is a letter, the identifier
  follows the same rules as for Ada identifiers, and can contain any letter,
  digit, or underscore character. An example of this is `"%ab_12"`. For
  readability, it is recommended to use the curly brace notation when the
  name is complex, but that is not mandatory.

* Otherwise the name of the identifier is the single character
  following the delimiter

For each substring matching the rules above, the `Substitute` subprogram
will look for possible replacement text in the following order:

* If the `Substrings` parameter contains an entry for that name,
  the corresponding value is used.

* Otherwise, if a `callback` was specified, it is called with the
  name of the identifier, and should return the appropriate substitution (or
  raise an exception if no such substitution makes sense).

* A default value provided in the substring itself

* When no replacement string was found, the substring is kept unmodified
