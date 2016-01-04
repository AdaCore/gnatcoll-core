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
