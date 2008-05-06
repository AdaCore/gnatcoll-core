-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  Various utility subprograms used in GNATCOLL, and that can easily be reused
--  elsewhere

with Ada.Calendar;
with GNAT.Strings;

package GNATCOLL.Utils is

   No_Time : constant Ada.Calendar.Time;
   --  A constant to indicate uninitialized time. Recent versions of GNAT
   --  provide this constant in the GNAT.Calendar package, but the constant is
   --  still defined here for compatibility with older compilers.
   --  Both constants must have the same value, though, because the user's code
   --  might be using one or the other indiscriminately.

   procedure Free (List : in out GNAT.Strings.String_List);
   --  Free the memory used by List.
   --  ??? This should be moved to GNAT.Strings itself in fact

   function Equal (S1, S2 : String; Case_Sensitive : Boolean) return Boolean;
   pragma Inline (Equal);
   --  Compare two strings

   function Image
     (Value      : Integer;
      Min_Width  : Integer;
      Force_Sign : Boolean := False;
      Padding    : Character := '0') return String;
   --  Return Value as a string, using at least Width digits (padded with
   --  leading characters Padding if necessary); negative values will always
   --  have a leading minus sign; positive values will have a leading plus sign
   --  if Force_Sign is True.
   --  If you set Min_Width to 1, the result is similar to 'Image, without the
   --  leading space for positive numbers.

private

   No_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of
     (Ada.Calendar.Year_Number'First,
      Ada.Calendar.Month_Number'First,
      Ada.Calendar.Day_Number'First);

end GNATCOLL.Utils;
