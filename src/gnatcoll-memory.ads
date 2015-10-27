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
--
--  As a child package of System, this package must be compiled with -gnatg
--  switch to the compiler.

with System; use System;
with GNAT.Debug_Pools; use GNAT.Debug_Pools;

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
      Low_Level_Traces               : Boolean           := False);
   --  Configure this package (these are global settings, not task-specific).
   --
   --  If Activate_Monitor is true, GPS will monitor all memory allocations and
   --  deallocations, and through the Dump procedure below be able to report
   --  the memory usage. The overhead is almost null when the monitor is
   --  disabled.
   --
   --  If Disable_Free is true, no deallocation is ever performed. This can be
   --  temporarily useful when investigating memory issues.
   --
   --    Stack_Trace_Depth. This parameter controls the maximum depth of stack
   --    traces that are output to indicate locations of actions for error
   --    conditions such as bad allocations. If set to zero, the debug pool
   --    will not try to compute backtraces. This is more efficient but gives
   --    less information on problem locations
   --
   --    Maximum_Logically_Freed_Memory: maximum amount of memory (bytes)
   --    that should be kept before starting to physically deallocate some.
   --    This value should be non-zero, since having memory that is logically
   --    but not physically freed helps to detect invalid memory accesses.
   --
   --    Minimum_To_Free is the minimum amount of memory that should be freed
   --    every time the pool starts physically releasing memory. The algorithm
   --    to compute which block should be physically released needs some
   --    expensive initialization (see Advanced_Scanning below), and this
   --    parameter can be used to limit the performance impact by ensuring
   --    that a reasonable amount of memory is freed each time. Even in the
   --    advanced scanning mode, marked blocks may be released to match this
   --    Minimum_To_Free parameter.
   --
   --    Reset_Content_On_Free: If true, then the contents of the freed memory
   --    is reset to the pattern 16#DEADBEEF#, following an old IBM convention.
   --    This helps in detecting invalid memory references from the debugger.
   --
   --    Raise_Exceptions: If true, the exceptions below will be raised every
   --    time an error is detected. If you set this to False, then the action
   --    is to generate output on standard error or standard output, depending
   --    on Errors_To_Stdout, noting the errors, but to
   --    keep running if possible (of course if storage is badly damaged, this
   --    attempt may fail. This helps to detect more than one error in a run.
   --
   --    Advanced_Scanning: If true, the pool will check the contents of all
   --    allocated blocks before physically releasing memory. Any possible
   --    reference to a logically free block will prevent its deallocation.
   --    Note that this algorithm is approximate, and it is recommended
   --    that you set Minimum_To_Free to a non-zero value to save time.
   --
   --    Errors_To_Stdout: Errors messages will be displayed on stdout if
   --    this parameter is True, or to stderr otherwise.
   --
   --    Low_Level_Traces: Traces all allocation and deallocations on the
   --    stream specified by Errors_To_Stdout. This can be used for
   --    post-processing by your own application, or to debug the
   --    debug_pool itself. The output indicates the size of the allocated
   --    block both as requested by the application and as physically
   --    allocated to fit the additional information needed by the debug
   --    pool.

   type Report_Type is new GNAT.Debug_Pools.Report_Type;

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

   type Byte_Count is new GNAT.Debug_Pools.Byte_Count;

   type Watermark_Info is record
      High    : Byte_Count;
      Current : Byte_Count;
   end record;

   function Get_Ada_Allocations return Watermark_Info;
   --  Return information about the allocations done from Ada.
   --  This does not include allocations done from other languages.

   function Get_Allocations return Watermark_Info;
   --  Return information about the allocations done in any language.
   --  This uses system calls to find out the program's resident size (RSS)
   --  information, both the peak and the current size.

private

   pragma Convention (C, Alloc);
   pragma Convention (C, Free);
   pragma Convention (C, Realloc);

end GNATCOLL.Memory;
