------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with Interfaces.C; use Interfaces.C;
with GNATCOLL.OS.FS;           use GNATCOLL.OS.FS;
with GNATCOLL.OS.Stat;
with GNATCOLL.OS.Libc;         use GNATCOLL.OS.Libc;
with GNATCOLL.OS.Libc.Utime;   use GNATCOLL.OS.Libc.Utime;
with Ada.Calendar;             use Ada.Calendar;
with Ada.Calendar.Conversions; use Ada.Calendar.Conversions;

separate (GNATCOLL.OS.FSUtil)
function Copy_Timestamps
  (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String) return Boolean
is

   function Set_Modif_And_Access_Time
     (File : UTF8.UTF_8_String; Modification : Time; Last_Access : Time)
      return Boolean;
   --  Set last modification and last access timestamp of specified file.
   --  Success is set to false if an error occurs

   function Set_Modif_And_Access_Time
     (File : UTF8.UTF_8_String; Modification : Time; Last_Access : Time)
      return Boolean
   is

      function To_Timespec (T : Time) return Timespec;
      --  Translate Time in Timespec

      function To_Timespec (T : Time) return Timespec is
         Nano           : constant                        := 1_000_000_000;
         Unix_Time_Nano : constant Interfaces.C.long_long :=
           To_Unix_Nano_Time (T);
         T_Spec         : Timespec;
      begin
         T_Spec.TV_Sec  := Interfaces.C.long (Unix_Time_Nano / Nano);
         T_Spec.TV_NSec := Interfaces.C.long (Unix_Time_Nano mod Nano);

         return T_Spec;
      end To_Timespec;

      Timespecs : Libc.Utime.Timespec_Array;
   begin

      Timespecs (1) := To_Timespec (Last_Access);
      Timespecs (2) := To_Timespec (Modification);

      declare
         Status : Libc_Status;
         FD     : File_Descriptor;
      begin
         FD := Open (File, Read_Mode);
         if FD = Invalid_FD then
            return False;
         end if;

         Status := Libc.Utime.Futimens (FD, Timespecs);
         Close (FD);
         if Status = Libc.Error then
            return False;
         end if;
      end;

      return True;

   end Set_Modif_And_Access_Time;

   File_Attr : File_Attributes;
begin

   File_Attr := GNATCOLL.OS.Stat.Stat (Src);
   if not Exists (File_Attr) then
      return False;
   end if;

   declare
      T : constant Time := Modification_Time (File_Attr);
   begin

      --  Last modification and access are set to the same timestamp

      return Set_Modif_And_Access_Time (Dst, T, T);
   end;
end Copy_Timestamps;
