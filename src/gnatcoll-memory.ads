------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

--  This package provides a reimplementation of GNAT's low-level memory
--  allocation mechanism. Its goal is to provide an additional monitoring
--  facility to check where your program allocates memory.
--
--  To activate this alternates implementation in your application, you
--  must provide your own s-memory.adb somewhere in your source directories.
--  Then simply recompile the modified body of that package with
--  gnatmake -u -a -g s-memory.adb (or use the -a switch when you compile
--  your own application) and make sure that the ali and object files for
--  this unit are found in the object search path.
--
--  This package is not task safe
--
--  Your version of System.Memory (in file s-memory.adb) should be:
--
--  with GNATCOLL.Memory;
--  package body System.Memory is
--     package M renames GNATCOLL.Memory;
--
--     function Alloc (Size : size_t) return System.Address is
--     begin
--        return M.Alloc (M.size_t (Size));
--     end Alloc;
--
--     procedure Free (Ptr : System.Address)
--        renames M.Free;
--
--     function Realloc
--        (Ptr  : System.Address;
--         Size : size_t)
--        return System.Address is
--     begin
--        return M.Realloc (Ptr, M.size_t (Size));
--     end Realloc;
--  end System.Memory;

with System; use System;

package GNATCOLL.Memory is
   type size_t is mod 2 ** Standard'Address_Size;
   --  Same as System.Memory.size_t, but defined here to avoid elaboration
   --  circularity issues

   function Alloc (Size : size_t) return System.Address;
   --  This is the low level allocation routine. Given a size in storage
   --  units, it returns the address of a maximally aligned block of
   --  memory. The implementation of this routine is guaranteed to be
   --  task safe, and also aborts are deferred if necessary.
   --
   --  If size_t is set to size_t'Last on entry, then a Storage_Error
   --  exception is raised with a message "object too large".
   --
   --  If size_t is set to zero on entry, then a minimal (but non-zero)
   --  size block is allocated.
   --
   --  Note: this is roughly equivalent to the standard C malloc call
   --  with the additional semantics as described above.

   procedure Free (Ptr : System.Address);
   --  This is the low level free routine. It frees a block previously
   --  allocated with a call to Alloc. As in the case of Alloc, this
   --  call is guaranteed task safe, and aborts are deferred.
   --
   --  Note: this is roughly equivalent to the standard C free call
   --  with the additional semantics as described above.

   function Realloc
     (Ptr  : System.Address;
      Size : size_t) return System.Address;
   --  This is the low level reallocation routine. It takes an existing
   --  block address returned by a previous call to Alloc or Realloc,
   --  and reallocates the block. The size can either be increased or
   --  decreased. If possible the reallocation is done in place, so that
   --  the returned result is the same as the value of Ptr on entry.
   --  However, it may be necessary to relocate the block to another
   --  address, in which case the information is copied to the new
   --  block, and the old block is freed. The implementation of this
   --  routine is guaranteed to be task safe, and also aborts are
   --  deferred as necessary.
   --
   --  If size_t is set to size_t'Last on entry, then a Storage_Error
   --  exception is raised with a message "object too large".
   --
   --  If size_t is set to zero on entry, then a minimal (but non-zero)
   --  size block is allocated.
   --
   --  Note: this is roughly equivalent to the standard C realloc call
   --  with the additional semantics as described above.

   -------------
   -- Monitor --
   -------------
   --  This package provides various optional monitoring capabilities. They
   --  are only activated if the environment variable GPS_MEMORY_MONITOR is
   --  set. If so, this package will check where memory is allocated and will
   --  be able to report where memory is mostly in use. You can query this
   --  information interactively through the Python console:
   --      GPS.debug_memory_usage (count);

   procedure Configure
     (Activate_Monitor  : Boolean := False;
      Disable_Free      : Boolean := False;
      Stack_Trace_Depth : Positive := 30);
   --  Configure this package (these are global settings, not task-specific).
   --  If Activate_Monitor is true, GPS will monitor all memory allocations and
   --  deallocations, and through the Dump procedure below be able to report
   --  the memory usage. The overhead is almost null when the monitor is
   --  disabled.
   --  If Disable_Free is true, no deallocation is ever performed. This can be
   --  temporarily useful when investigating memory issues.

   type Report_Type is
     (All_Reports,
      Memory_Usage,
      Allocations_Count,
      Sort_Total_Allocs,
      Marked_Blocks);
   for Report_Type use
     (All_Reports       => 0,
      Memory_Usage      => 1,
      Allocations_Count => 2,
      Sort_Total_Allocs => 3,
      Marked_Blocks     => 4);

   procedure Dump (Size : Positive; Report : Report_Type := All_Reports);
   --  Dump information about memory usage.
   --  Size is the number of the biggest memory users we want to show. Report
   --  indicates which sorting order is used in the report

   procedure Reset;
   --  Reset all internal data. This is in general not needed, unless you want
   --  to know what memory is used by specific parts of your application

   procedure Mark_Traceback;
   --  Add a special chunk in the monitor for the current traceback. This is
   --  a convenient way to check how many times we go through a given path,
   --  and where this is called from.
   --  Nothing is done if the memory monitor has not been activated

private

   pragma Convention (C, Alloc);
   pragma Convention (C, Free);
   pragma Convention (C, Realloc);

end GNATCOLL.Memory;
