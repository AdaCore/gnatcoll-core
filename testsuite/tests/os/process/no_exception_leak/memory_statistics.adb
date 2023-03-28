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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Address_Image;
with GNATCOLL.Memory;
with Ada.Calendar;             use Ada.Calendar;
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;  use Ada.Calendar.Time_Zones;

package body Memory_Statistics is

   ----------------------------
   -- Dump_Memory_Statistics --
   ----------------------------

   function Dump_Memory_Statistics
     (Comment : String;
      Size : Positive;
      Report : Report_Type := All_Reports)
      return String
   is

      procedure Trace_Put (S : String);
      procedure Trace_Put_Line (S : String);

      Buffer : Unbounded_String := To_Unbounded_String
        (Image
           (Date => Clock,
            Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset) &
         " " & Comment & " - Dump_Memory_Statistics at 0x" &
         System.Address_Image (Dump_Memory_Statistics'Address) & ASCII.LF);

      procedure Trace_Put (S : String) is
      begin
         Append (Buffer, S);
      end Trace_Put;

      procedure Trace_Put_Line (S : String) is
      begin
         Append (Buffer, S & ASCII.LF);
      end Trace_Put_Line;

      procedure Internal is new GNATCOLL.Memory.Redirectable_Dump
        (Put_Line => Trace_Put_Line,
         Put      => Trace_Put);

   begin
      case Report is
         when Memory_Usage =>
            Internal (Size, GNATCOLL.Memory.Memory_Usage);
         when Allocations_Count =>
            Internal (Size, GNATCOLL.Memory.Allocations_Count);
         when Sort_Total_Allocs =>
            Internal (Size, GNATCOLL.Memory.Sort_Total_Allocs);
         when Marked_Blocks =>
            Internal (Size, GNATCOLL.Memory.Marked_Blocks);
         when others =>
            Internal (Size);
      end case;
      return To_String (Buffer);
   end Dump_Memory_Statistics;

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
      GNATCOLL.Memory.Configure
        (Activate_Monitor               => Activate_Monitor,
         Disable_Free                   => Disable_Free,
         Stack_Trace_Depth              => Stack_Trace_Depth,
         Maximum_Logically_Freed_Memory => Maximum_Logically_Freed_Memory,
         Minimum_To_Free                => Minimum_To_Free,
         Reset_Content_On_Free          => Reset_Content_On_Free,
         Raise_Exceptions               => Raise_Exceptions,
         Advanced_Scanning              => Advanced_Scanning,
         Errors_To_Stdout               => Errors_To_Stdout,
         Low_Level_Traces               => Low_Level_Traces);

   end Configure;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      GNATCOLL.Memory.Reset;
   end Reset;

   -------------------------
   -- Get_Ada_Allocations --
   -------------------------

   function Get_Ada_Allocations return Watermark_Info is
      Ada_Allocations : constant GNATCOLL.Memory.Watermark_Info :=
        GNATCOLL.Memory.Get_Ada_Allocations;
   begin
      return
        (High    => Byte_Count (Ada_Allocations.High),
         Current => Byte_Count (Ada_Allocations.Current)
        );
   end Get_Ada_Allocations;

   ---------------------
   -- Get_Allocations --
   ---------------------

   function Get_Allocations return Watermark_Info is
      function Get_Peak_RSS return GNATCOLL.Memory.size_t;
      pragma Import (C, Get_Peak_RSS, "gnatcoll_getPeakRSS");

      function Get_Current_RSS return GNATCOLL.Memory.size_t;
      pragma Import (C, Get_Current_RSS, "gnatcoll_getCurrentRSS");

   begin
      return (High    => Byte_Count (Get_Peak_RSS),
              Current => Byte_Count (Get_Current_RSS));
   end Get_Allocations;

end Memory_Statistics;
