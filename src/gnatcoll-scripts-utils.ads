-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2003-2009, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

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
