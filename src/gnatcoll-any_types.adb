------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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

package body GNATCOLL.Any_Types is

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Any_Type) is
   begin
      case X.T is
         when Integer_Type | String_Type | No_Type =>
            --  Nothing to free for these types
            null;
         when List_Type =>
            for J in X.List'Range loop
               Free (X.List (J).all);
               Unchecked_Free (X.List (J));
            end loop;
         when Tuple_Type =>
            for J in X.Tuple'Range loop
               Free (X.Tuple (J).all);
               Unchecked_Free (X.Tuple (J));
            end loop;
      end case;
   end Free;

end GNATCOLL.Any_Types;
