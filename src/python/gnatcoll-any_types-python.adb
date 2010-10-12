-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

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
