-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                      Copyright (C) 2009, AdaCore                  --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  An Ada interface to the GNU Multiple Precision (GMP) arithmentic library.
--  See child packages for specific types, such as package GMP.Integers.

with Interfaces.C;

package GNATCOLL.GMP is

   pragma Pure;

   --  We define these numeric types here so that clients of the Ada binding
   --  do not also have to import package Interfaces.C themselves.
   --  These types correspond to those used by the underlying C implementation
   --  of the GMP library itself.

   type Int is new Interfaces.C.int;

   type Long is new Interfaces.C.long;

   type Unsigned_Long is new Interfaces.C.unsigned_long;

end GNATCOLL.GMP;
