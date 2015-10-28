------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2015, AdaCore                     --
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

with Ada.Exceptions;
with System.Storage_Elements;  use System.Storage_Elements;

pragma Warnings (Off);
with System.CRTL;

--  Force a user's s-memory.adb to be part of the link,
--  otherwise s-memory.o will simply be ignored by gprbuild
with System.Memory;

pragma Warnings (On);

package body GNATCOLL.Memory is

   Memory_Pool : GNAT.Debug_Pools.Debug_Pool;

   use Ada.Exceptions;

   Memory_Monitor : Boolean := False;
   Memory_Check   : Boolean := False;
   Debug_Pool_Initialization_Needed : Boolean := True;

   procedure Initialize_System_Memory_Debug_Pool
     (Has_Unhandled_Memory : Boolean := False);
   --  If not already done, let Debug_Pools know that System.Memory will use
   --  Debug_Pools, and have to handle memory allocated by System.CRTL package.

   -----------------------------------------
   -- Initialize_System_Memory_Debug_Pool --
   -----------------------------------------

   procedure Initialize_System_Memory_Debug_Pool
     (Has_Unhandled_Memory : Boolean := False) is
   begin
      if Debug_Pool_Initialization_Needed then
         Debug_Pool_Initialization_Needed := False;
         GNAT.Debug_Pools.System_Memory_Debug_Pool (Has_Unhandled_Memory);
      end if;
   end Initialize_System_Memory_Debug_Pool;

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

      if Memory_Monitor then

         Initialize_System_Memory_Debug_Pool;

         Memory_Pool.Allocate
           (Storage_Address          => Result,
            Size_In_Storage_Elements => Storage_Count (Actual_Size),
            Alignment                => Standard'Maximum_Alignment);

      else
         Result := System.CRTL.malloc (System.CRTL.size_t (Actual_Size));
      end if;

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

      if Ptr /= System.Null_Address and not Memory_Check then
         if Memory_Monitor then

            Initialize_System_Memory_Debug_Pool;

            Memory_Pool.Deallocate
              (Storage_Address          => Ptr,
               Size_In_Storage_Elements => Storage_Count'Last,
               Alignment                => Standard'Maximum_Alignment);
         else
            System.CRTL.free (Ptr);
         end if;

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
      Actual_Size : size_t := Size;
   begin

      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      if not Memory_Monitor then
         Result := System.CRTL.realloc (Ptr, System.CRTL.size_t (Actual_Size));
      else
         declare
            Size_Was    : Storage_Count;
            Valid       : Boolean;

            procedure Memmove
              (Dest : System.Address;
               Src  : System.Address;
               N    : size_t);
            pragma Import (C, Memmove, "memmove");

         begin

            Initialize_System_Memory_Debug_Pool;

            Get_Size
              (Storage_Address          => Ptr,
               Size_In_Storage_Elements => Size_Was,
               Valid                    => Valid);
            if not Valid then
               declare
                  Reallocated : System.Address;
               begin
                  Reallocated := System.CRTL.realloc
                    (Ptr, System.CRTL.size_t (Actual_Size));
                  Memory_Pool.Allocate
                    (Storage_Address          => Result,
                     Size_In_Storage_Elements => Storage_Count (Actual_Size),
                     Alignment                => Standard'Maximum_Alignment);
                  Memmove
                   (Dest => Result,
                    Src  => Reallocated,
                    N    => Actual_Size);
                  System.CRTL.free (Reallocated);
               end;
            else
               Memory_Pool.Allocate
                 (Storage_Address          => Result,
                  Size_In_Storage_Elements => Storage_Count (Size),
                  Alignment                => Standard'Maximum_Alignment);
               if size_t (Size_Was) < Actual_Size then
                  Actual_Size := size_t (Size_Was);
               end if;
               Memmove
                 (Dest => Result,
                  Src  => Ptr,
                  N    => Actual_Size);
               Free (Ptr);
            end if;

         end;

      end if;

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      return Result;
   end Realloc;

   ----------
   -- Dump --
   ----------

   procedure Dump (Size : Positive; Report : Report_Type := All_Reports) is
   begin

      Memory_Pool.Dump_Stdout
        (Size   => Size,
         Report => GNAT.Debug_Pools.Report_Type (Report));
   end Dump;

   -------------------------
   -- Get_Ada_Allocations --
   -------------------------

   function Get_Ada_Allocations return Watermark_Info is
   begin
      return
        (High    => GNATCOLL.Memory.Byte_Count (Memory_Pool.High_Water_Mark),
         Current => GNATCOLL.Memory.Byte_Count (Memory_Pool.Current_Water_Mark)
        );
   end Get_Ada_Allocations;

   ---------------------
   -- Get_Allocations --
   ---------------------

   function Get_Allocations return Watermark_Info is
      function Get_Peak_RSS return size_t;
      pragma Import (C, Get_Peak_RSS, "gnatcoll_getPeakRSS");

      function Get_Current_RSS return size_t;
      pragma Import (C, Get_Current_RSS, "gnatcoll_getCurrentRSS");

   begin
      return (High    => Byte_Count (Get_Peak_RSS),
              Current => Byte_Count (Get_Current_RSS));
   end Get_Allocations;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      GNAT.Debug_Pools.Reset;
   end Reset;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Activate_Monitor               : Boolean           := False;
      Disable_Free                   : Boolean           := False;
      Stack_Trace_Depth              : Natural           := 30;
      Maximum_Logically_Freed_Memory : Long_Long_Integer := 50_000_000;
      Minimum_To_Free                : Long_Long_Integer  := 0;
      Reset_Content_On_Free          : Boolean           := True;
      Raise_Exceptions               : Boolean           := False;
      Advanced_Scanning              : Boolean           := False;
      Errors_To_Stdout               : Boolean           := True;
      Low_Level_Traces               : Boolean           := False) is
   begin

      Memory_Check := Disable_Free;

      if Activate_Monitor and not Memory_Monitor then
         Initialize_System_Memory_Debug_Pool
           (Has_Unhandled_Memory => True);
         Memory_Monitor := True;
      end if;

      if Memory_Monitor then

         Memory_Pool.Configure
           (Stack_Trace_Depth              => Stack_Trace_Depth,
            Maximum_Logically_Freed_Memory =>
              SSC (Maximum_Logically_Freed_Memory),
            Minimum_To_Free                => SSC (Minimum_To_Free + 1),
            Reset_Content_On_Free          => Reset_Content_On_Free,
            Raise_Exceptions               => Raise_Exceptions,
            Advanced_Scanning              => Advanced_Scanning,
            Errors_To_Stdout               => Errors_To_Stdout,
            Low_Level_Traces               => Low_Level_Traces);
      end if;

   end Configure;

   --------------------
   -- Mark_Traceback --
   --------------------

   procedure Mark_Traceback is
      Size_Was : Byte_Count;
      pragma Unreferenced (Size_Was);
   begin
      null;
   end Mark_Traceback;

end GNATCOLL.Memory;
