------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . M E M O R Y                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2016, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Copied from System.Memory, but counts the number of times we do
--  malloc and realloc.

with Ada.Exceptions;
with System.Soft_Links;
with System.Parameters;
with System.CRTL;
with Memory;

package body System.Memory is

   use Ada.Exceptions;
   use System.Soft_Links;

   function c_malloc (Size : System.CRTL.size_t) return System.Address
    renames System.CRTL.malloc;

   procedure c_free (Ptr : System.Address)
     renames System.CRTL.free;

   function c_realloc
     (Ptr : System.Address; Size : System.CRTL.size_t) return System.Address
     renames System.CRTL.realloc;

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return System.Address is
      Result      : System.Address;
      Actual_Size : size_t := Size;

   begin
      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      --  Change size from zero to non-zero. We still want a proper pointer
      --  for the zero case because pointers to zero length objects have to
      --  be distinct, but we can't just go ahead and allocate zero bytes,
      --  since some malloc's return zero for a zero argument.

      if Size = 0 then
         Actual_Size := 1;
      end if;

      if Parameters.No_Abort then
         Result := c_malloc (System.CRTL.size_t (Actual_Size));
      else
         Abort_Defer.all;
         Result := c_malloc (System.CRTL.size_t (Actual_Size));
         Abort_Undefer.all;
      end if;

      Standard.Memory.Allocs := Standard.Memory.Allocs + 1;

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      return Result;
   end Alloc;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : System.Address) is
   begin
      if Parameters.No_Abort then
         c_free (Ptr);
      else
         Abort_Defer.all;
         c_free (Ptr);
         Abort_Undefer.all;
      end if;
   end Free;

   -------------
   -- Realloc --
   -------------

   function Realloc
     (Ptr  : System.Address;
      Size : size_t)
      return System.Address
   is
      Result      : System.Address;
      Actual_Size : constant size_t := Size;

   begin
      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      if Parameters.No_Abort then
         Result := c_realloc (Ptr, System.CRTL.size_t (Actual_Size));
      else
         Abort_Defer.all;
         Result := c_realloc (Ptr, System.CRTL.size_t (Actual_Size));
         Abort_Undefer.all;
      end if;

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      Standard.Memory.Reallocs := Standard.Memory.Reallocs + 1;

      return Result;
   end Realloc;

end System.Memory;
