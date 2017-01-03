------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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

package body GNATCOLL.Any_Types.Python is

   -------------------
   -- From_PyObject --
   -------------------

   function From_PyObject (Object : PyObject) return Any_Type is
   begin
      if Object = null
        or else Object = Py_None
      then
         return Empty_Any_Type;
      end if;

      if PyInt_Check (Object) then
         declare
            A : Any_Type (Integer_Type, 0);
         begin
            A.Int := PyInt_AsLong (Object);
            return A;
         end;
      elsif PyString_Check (Object) then
         declare
            S : constant String := PyString_AsString (Object);
            A : Any_Type (String_Type, S'Length);
         begin
            A.Str := S;
            return A;
         end;
      elsif PyUnicode_Check (Object) then
         declare
            S : constant String := Unicode_AsString (Object);
            A : Any_Type (String_Type, S'Length);
         begin
            A.Str := S;
            return A;
         end;
      elsif PyList_Check (Object) then
         declare
            Size : constant Integer := PyList_Size (Object);
            Arr  : Any_Type_Array (1 .. Size);
            A    : Any_Type (List_Type, Size);
         begin
            for J in 1 .. Size loop
               Arr (J) := new Any_Type'
                 (From_PyObject (PyList_GetItem (Object, J - 1)));
            end loop;
            A.List := Arr;
            return A;
         end;
      elsif PyTuple_Check (Object) then
         declare
            Size : constant Integer := PyTuple_Size (Object);
            Arr  : Any_Type_Array (1 .. Size);
            A    : Any_Type (Tuple_Type, Size);
         begin
            for J in 1 .. Size loop
               Arr (J) := new Any_Type'
                 (From_PyObject (PyTuple_GetItem (Object, J - 1)));
            end loop;
            A.Tuple := Arr;
            return A;
         end;
      else
         --  When adding support for new types, add the corresponding cases
         --  here.

         null;
      end if;

      return Empty_Any_Type;
   end From_PyObject;

end GNATCOLL.Any_Types.Python;
