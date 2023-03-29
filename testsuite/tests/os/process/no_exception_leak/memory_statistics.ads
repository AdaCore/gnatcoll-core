------------------------------------------------------------------------------
--                              GNATBENCH                                   --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNAT.Debug_Pools;

pragma Warnings (Off);
with System.Memory;
pragma Warnings (On);

package Memory_Statistics is

   type Report_Type is
     (All_Reports,
      Memory_Usage,
      Allocations_Count,
      Sort_Total_Allocs,
      Marked_Blocks);

   function Dump_Memory_Statistics
     (Comment : String;
      Size : Positive;
      Report : Report_Type := All_Reports) return String;
   --  see GNATCOLL.Memory.Dump

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
   --  see GNATCOLL.Memory.Configure

   procedure Reset;
   --  see GNATCOLL.Reset

   type Byte_Count is new GNAT.Debug_Pools.Byte_Count;

   type Watermark_Info is record
      High    : Byte_Count;
      Current : Byte_Count;
   end record;

   function Get_Ada_Allocations return Watermark_Info;
   --  see GNATCOLL.Get_Ada_Allocations

   function Get_Allocations return Watermark_Info;
   --  see GNATCOLL.Get_Allocations

end Memory_Statistics;
