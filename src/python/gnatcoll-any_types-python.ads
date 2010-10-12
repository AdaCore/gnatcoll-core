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

--  This package provides a utilities to manipulate Python objects. This is not
--  meant to be very performance-efficient, but to provide an interface simpler
--  than the direct manipulation of PyObjects.

with GNATCOLL.Python; use GNATCOLL.Python;

package GNATCOLL.Any_Types.Python is

   function From_PyObject (Object : PyObject) return Any_Type;
   --  Create an Any_Type from the contents of Object. This creates copies in
   --  of any data in Object.
   --  Empty_Any_Type is returned if the underlying Python type (or its
   --  children in case of container types) is not supported.
   --  The result must be freed by the caller, by calling Free.

end GNATCOLL.Any_Types.Python;
