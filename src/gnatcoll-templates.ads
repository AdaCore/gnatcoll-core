------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

--  This package provides support for replacing special substrings in a string.
--  Typically, these are used to replace substrings like "%version" by the some
--  other value, at run time.
--  Do not mix this unit with the templates parser which is provided in the
--  context of the Ada Web Server (AWS), and which is used to parse external
--  file and replace part of them.

with GNAT.Strings;

package GNATCOLL.Templates is

   type Substitution_Value is record
      Name  : GNAT.Strings.String_Access;
      Value : GNAT.Strings.String_Access;
   end record;
   type Substitution_Array is array (Natural range <>) of Substitution_Value;

   procedure Free (Substrings : in out Substitution_Array);
   --  Free the memory occupied by the array

   No_Substitution : constant Substitution_Array;

   type Substitute_Callback is access
     function (Name   : String;
               Quoted : Boolean) return String;
   --  A callback for Substitute below. It is called once for each '%...'
   --  parameter found in the string. Name doesn't include the delimiter.
   --  Quoted indicate whether the parameter was quoted, ie the '%...' was
   --  found as part of a quoted subtrings (for instance
   --       a "quoted %version substring" b
   --  The reason is that the substituted version could be different in such
   --  a case, and the subtituted value might need to protect quote symbols
   --  in its replacement string.
   --  Should raise Invalid_Substitution if Name cannot be substituted

   Default_Delimiter : constant Character := '%';
   --  The default delimiter used to mark the special substrings. It can be
   --  overriden in the various Substitute subprograms below.
   --  The special substrings always start with this delimiter, and including
   --  the following number or identifier. That identifier can be quoted
   --  between curly braces ({...}) or parenthesis to avoid ambiguities.
   --  For instance:
   --    a%bcd*df    =>  identifier name is "bcd"
   --    a%123ab     =>  identifier name is "123"
   --    a%{bc}d*df  =>  identifier name is "bc"
   --    a%(bc)d*df  =>  identifier name is "bc"
   --
   --  If the identifier is a number, the special character "-" will
   --  be included in the name if it follows the number exactly:
   --     a %1- b   => identifier name is "1-"
   --     a %1+ b   => identifier name is "1"
   --  The goal is to use this to indicate a range of parameter (in the first
   --  example above, the intended substitution is the value of %1 concatenated
   --  with that of %2, %3,..., so is an equivalent of "%1%2%3%4" if there are
   --  four possible parameters.
   --
   --  If the first character after the delimiter is not an alphanumeric
   --  character, that will be the name of the identifier
   --     a %* b   => identifier name is "*"
   --     a %^ b   => identifier name is "^"
   --
   --  When the delimiter is duplicated, it will always be replaced by a
   --  single instance of the delimiter (unless you have specified another
   --  explicit replacement for it). For instance, if the string contains
   --    "a%%b" it will be replaced with "a%b".
   --
   --  When an identifier is specified within curly braces or parenthesis, a
   --  default value can be specified for it, which will be used if no other
   --  substitution is available. The syntax is similar to that of the Unix
   --  shell:
   --     %{var:-default}
   --  where "default" is the default value to use.

   type Error_Handling is (Keep_As_Is,
                           Replace_With_Empty,
                           Report_Error);
   --  What to do when no substitution value was found:
   --    If Keep_As_Is, the text is unaltered. "%invalid" remains as is
   --    If Replace_With_Empty, the text is replaced with the empty string.
   --        "%invalid" becomes "".
   --    If Report_Error, an exception Invalid_Substitution is raised

   function Substitute
     (Str          : String;
      Substrings   : Substitution_Array := No_Substitution;
      Callback     : Substitute_Callback := null;
      Delimiter    : Character := Default_Delimiter;
      Recursive    : Boolean := False;
      Errors       : Error_Handling := Keep_As_Is) return String;
   --  Replace all substrings in Str that start with Delimiter (see the
   --  declaration of Default_Delimiter for more information on identifier
   --  names).
   --  If an identifier found in Str matches no entry from Substrings,
   --  Callback is called to try and find the appropriate substitution. If
   --  that raises Invalid_Substitution, and the identifier contains a default
   --  value, it is used.
   --  If no substitution value was found, the behavior depends on the
   --  Error parameter.
   --
   --  If Recursive is true, then this function will also substitute substrings
   --  in the values specified in Substrings, for instance:
   --      Delimiter := %
   --      Substrings (1) := (Name => "a", Value => "c%b")
   --      Substrings (2) := (Name => "b", Value => "d")
   --
   --      Str := "%a"   results in   "cd"  if Recursive is True
   --                    results in   "c%b" otherwise

   Invalid_Substitution : exception;

private
   No_Substitution : constant Substitution_Array := (1 .. 0 => (null, null));
end GNATCOLL.Templates;
