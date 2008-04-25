-----------------------------------------------------------------------
--                               G N A T C O L L                     --
--                                                                   --
--                 Copyright (C) 2008, AdaCore                       --
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

with GNAT.Strings;

package GNATCOLL.Utils is

   procedure Free (List : in out GNAT.Strings.String_List);
   --  Free the memory used by List.
   --  ??? This should be moved to GNAT.Strings itself in fact

   function Equal (S1, S2 : String; Case_Sensitive : Boolean) return Boolean;
   pragma Inline (Equal);
   --  Compare two strings

end GNATCOLL.Utils;
