------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Strings;

package GNATCOLL.Scripts.Utils is

   function Argument_List_To_Quoted_String
     (Args            : GNAT.Strings.String_List;
      Quote           : Character := '"';
      Quote_Backslash : Boolean := True) return String;
   --  Return the arguments as a full string.
   --  Arguments that contain spaces but do not already contain quotes
   --  will be put into quotes.
   --  Backslashes are duplicated if Quote_Baskslash is True.
   --  The result of this subprogram on the string     A simple\ "string"
   --  is:     Quote_Backslash =>   "A simple\\ \"string\""
   --      not Quote_Backslash =>   "A simple\ \"string\""

   function Argument_To_Quoted_String
     (Arg             : String;
      Quote           : Character := '"';
      Quote_Backslash : Boolean := True) return String;
   --  As above but for a single argument

   function Argument_String_To_List_With_Triple_Quotes
     (Arg_String : String) return GNAT.Strings.String_List_Access;
   --  This is similar to GNAT.OS_Lib.Argument_String_To_List, except that
   --  if part of the string is surrounded by triple quotes, any special
   --  character is ignored till the closing triple quotes. This is the same
   --  behavior as in Python, and is needed for easier quoting of string.
   --
   --  Here is the output in some cases:
   --     "foo"       -> "foo"       (quotes preserved)
   --     """foo"""   -> foo         (quotes removed when at beginning and end)
   --     ("""foo""") -> ("""foo""") (quotes preserved in middle)
   --     foo\"foo    -> foo\"foo    (backslash not removed from output)

   function Unprotect (Str : String) return String;
   --  Remove the \ protections in Str
   --  ??? This seems to also remove the quotes around an argument

end GNATCOLL.Scripts.Utils;
