------------------------------------------------------------------------------
--                               G N A T L I B                              --
--                                                                          --
--                      Copyright (C) 2001-2007, AdaCore                    --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Task_Lock;            use GNAT.Task_Lock;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

package body GNAT.Traces.Syslog is

   C_Prefix   : chars_ptr := Null_Ptr;
   --  This variable belongs to the package because on some systems,
   --  most notably Linux, openlog() does not copy the content of
   --  ident. As a consequence, we can't deallocate it until the closelog().
   --  Any access to these variables is protected through the use of
   --  Lock_Task/Unlock_Task, in order to have locking feature without
   --  getting the whole runtime.

   Facilities_To_Integer : constant array (Facilities) of Integer :=
     (Kernel    => 0  * 8,
      User      => 1  * 8,
      Mail      => 2  * 8,
      Daemon    => 3  * 8,
      Auth      => 4  * 8,
      Sys_Log   => 5  * 8,
      Printer   => 6  * 8,
      News      => 7  * 8,
      UUCP      => 8  * 8,
      Cron      => 9  * 8,
      Auth_Priv => 10 * 8,
      FTP       => 11 * 8,
      NTP       => 12 * 8,
      Security  => 13 * 8,
      Console   => 14 * 8,
      Local0    => 16 * 8,
      Local1    => 17 * 8,
      Local2    => 18 * 8,
      Local3    => 19 * 8,
      Local4    => 20 * 8,
      Local5    => 21 * 8,
      Local6    => 22 * 8,
      Local7    => 23 * 8);
   Levels_To_Integer : constant array (Levels) of Integer :=
     (Emergency => 0,
      Alert     => 1,
      Critical  => 2,
      Error     => 3,
      Warning   => 4,
      Notice    => 5,
      Info      => 6,
      Debug     => 7);
   --  Convert Facilities to integers. Do not use representation clauses for
   --  two reasons:
   --     They would have to appear in the public part of the spec, which is
   --       not nice
   --     We cannot convert a value to an integer through an attribute, and
   --       we need to be able to combine levels and facilities in the call
   --       to syslog...

   Percent_S : constant String := "%s" & ASCII.NUL;

   type Syslog_Stream_Record is new Trace_Stream_Record with record
      Buffer   : Unbounded_String;
      Facility : Facilities;
      Level    : Levels;
   end record;
   overriding procedure Put
     (Stream : in out Syslog_Stream_Record; Str : String);
   overriding procedure Newline (Stream : in out Syslog_Stream_Record);
   overriding function Supports_Color
     (Stream : Syslog_Stream_Record) return Boolean;
   overriding function Supports_Time
     (Stream : Syslog_Stream_Record) return Boolean;
   --  See inherited documentation

   type Factory is new Stream_Factory with null record;
   overriding function New_Stream
      (Fact : Factory; Args : String) return Trace_Stream;
   --  Create a syslog stream

   ---------
   -- Put --
   ---------

   procedure Put (Stream : in out Syslog_Stream_Record; Str : String) is
   begin
      Append (Stream.Buffer, Str);
   end Put;

   -------------
   -- Newline --
   -------------

   procedure Newline (Stream : in out Syslog_Stream_Record) is
   begin
      Syslog (Stream.Facility, Stream.Level, To_String (Stream.Buffer));
      Stream.Buffer := Null_Unbounded_String;
   end Newline;

   --------------------
   -- Supports_Color --
   --------------------

   function Supports_Color (Stream : Syslog_Stream_Record) return Boolean is
      pragma Unreferenced (Stream);
   begin
      return False;
   end Supports_Color;

   -------------------
   -- Supports_Time --
   -------------------

   function Supports_Time (Stream : Syslog_Stream_Record) return Boolean is
      pragma Unreferenced (Stream);
   begin
      return False;
   end Supports_Time;

   ----------------
   -- New_Stream --
   ----------------

   function New_Stream
      (Fact : Factory; Args : String) return Trace_Stream
   is
      pragma Unreferenced (Fact);
      Colon    : constant Integer := Index (Args, ":");
      Facility : Facilities := User;
      Level    : Levels     := Info;
   begin
      if Args /= ""  then
         if Colon < Args'First then
            Facility := Facilities'Value (Args);
         else
            Facility := Facilities'Value (Args (Args'First .. Colon - 1));
            Level    := Levels'Value (Args (Colon + 1 .. Args'Last));
         end if;
      end if;

      return new Syslog_Stream_Record'
        (Trace_Stream_Record with
         Buffer   => Null_Unbounded_String,
         Facility => Facility,
         Level    => Level);
   end New_Stream;

   ----------------------------
   -- Register_Syslog_Stream --
   ----------------------------

   procedure Register_Syslog_Stream is
      Fact : constant Stream_Factory_Access := new Factory;
   begin
      Openlog (Base_Name (Ada.Command_Line.Command_Name),
               Customization    => PID,
               Default_Facility => User);
      Register_Stream_Factory (Stream_Syslog, Fact);
   end Register_Syslog_Stream;

   -------------
   -- Openlog --
   -------------

   procedure Openlog
     (Prefix           : String;
      Customization    : Options;
      Default_Facility : Facilities)
   is
      procedure Internal
        (Prefix        : chars_ptr;
         Customization : Options;
         Facility      : Integer);
      pragma Import (C, Internal, "openlog");
      --  Low-level binding

   begin
      Lock;
      if C_Prefix /= Null_Ptr then
         Free (C_Prefix);
      end if;
      C_Prefix := New_String (Prefix);
      Internal
        (C_Prefix, Customization, Facilities_To_Integer (Default_Facility));
      Unlock;
   end Openlog;

   ------------
   -- Syslog --
   ------------

   procedure Syslog
     (Facility : Facilities := Kernel;
      Level    : Levels     := Emergency;
      Message  : String)
   is
      procedure Internal
        (Priority : Integer;
         Message  : String;
         Arg      : String);
      pragma Import (C, Internal, "syslog");
      --  Low-level binding
   begin
      Internal (Levels_To_Integer (Level) + Facilities_To_Integer (Facility),
                Percent_S,
                Message & ASCII.NUL);
   end Syslog;

   --------------
   -- Closelog --
   --------------

   procedure Closelog is
      procedure Internal;
      pragma Import (C, Internal, "closelog");
      --  Low-level binding
   begin
      Lock;
      Internal;
      if C_Prefix /= Null_Ptr then
         Free (C_Prefix);
      end if;
      Unlock;
   end Closelog;

end GNAT.Traces.Syslog;
