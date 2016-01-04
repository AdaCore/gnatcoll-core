------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2016, AdaCore                     --
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

--  This package provides a few utilities to manipulate types that can have
--  multiple forms. In particular this is used to provide a convenient way
--  of manipulating Python objects, without having the need to manipulate
--  PyObject in the Ada code.
--  See GNATCOLL.Any_Types.Python.

with Ada.Unchecked_Deallocation;
with Interfaces.C;

package GNATCOLL.Any_Types is

   --------------
   -- Any_Type --
   --------------

   --  This type provides an Ada encapsulation of certain types

   type Types is (No_Type,
                  Integer_Type,
                  String_Type,
                  Tuple_Type,
                  List_Type);

   type Any_Type;
   type Any_Type_Access is access Any_Type;

   type Any_Type_Array is array (Natural range <>) of Any_Type_Access;

   type Any_Type (T : Types; Length : Natural) is record
      case T is
         when No_Type =>
            null;
         when Integer_Type =>
            Int : Interfaces.C.long;
         when String_Type =>
            Str : String (1 .. Length);
         when Tuple_Type =>
            Tuple : Any_Type_Array (1 .. Length);
         when List_Type =>
            List  : Any_Type_Array (1 .. Length);
      end case;
   end record;

   procedure Free (X : in out Any_Type);
   --  Free memory associated to X

   Empty_Any_Type : constant Any_Type := (No_Type, 0);

private

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Any_Type, Any_Type_Access);

end GNATCOLL.Any_Types;
