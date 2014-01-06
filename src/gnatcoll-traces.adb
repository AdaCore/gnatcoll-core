------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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

pragma Ada_2012;

with Ada.Calendar;              use Ada.Calendar;
with Ada.Calendar.Formatting;   use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;   use Ada.Calendar.Time_Zones;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Calendar;             use GNAT.Calendar;
with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNATCOLL.Mmap;             use GNATCOLL.Mmap;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Task_Lock;            use GNAT.Task_Lock;
with GNAT.Traceback;            use GNAT.Traceback;

with System.Address_Image;
with System.Assertions;         use System.Assertions;
pragma Warnings (Off);
with System.Traceback_Entries;  use System.Traceback_Entries;

with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;

pragma Warnings (On);

package body GNATCOLL.Traces is

   On_Exception : On_Exception_Mode := Propagate;
   --  The behavior that should be adopted when something unexpected prevent
   --  the log stream to be written.

   No_Time : constant Ada.Calendar.Time :=
               Ada.Calendar.Time_Of
                 (Ada.Calendar.Year_Number'First,
                  Ada.Calendar.Month_Number'First,
                  Ada.Calendar.Day_Number'First);
   --  Note: we can remove this constant once we require GNAT 6.1 to build
   --  GPS.

   --  Note: rev 1.5 of this file has a (disabled) support for symbolic
   --  tracebacks.

   --  ??? We could display the stack pointer with
   --  procedure Print_Sp is
   --     start : aliased Integer;
   --  begin
   --     Put_Line (System.Address_Image (Start'Address));
   --  end;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Trace_Handle_Record'Class, Trace_Handle);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Trace_Stream_Record'Class, Trace_Stream);

   type Stream_Factories;
   type Stream_Factories_List is access Stream_Factories;
   type Stream_Factories is record
      Name    : GNAT.Strings.String_Access;
      Factory : Stream_Factory_Access;
      Next    : Stream_Factories_List;
   end record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Stream_Factories, Stream_Factories_List);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Stream_Factory'Class, Stream_Factory_Access);

   type Global_Vars is record
      Handles_List : Trace_Handle := null;
      --  The global list of all defined handles.
      --  Accesses to this list are protected by calls to
      --  System.Soft_Links.Lock_Task (we do not use a protected type so that
      --  applications that do not use tasking otherwise do not drag the whole
      --  tasking runtime in).

      Wildcard_Handles_List : Trace_Handle := null;
      --  Contains the configuration for module names containing stars, for
      --  instance "*.EXCEPTIONS".

      Streams_List : Trace_Stream := null;
      --  The global list of all streams. Accesses to this list are protected
      --  by calls to System.Soft_Links.Lock_Task.  The default stream is the
      --  first in the list.

      Factories_List : Stream_Factories_List := null;
      --  The global list of all factories. Access to this list are protected
      --  by calls to Lock_Task

      Default_Activation : Boolean := False;
      --  Default activation status for debug handles (ie whether the
      --  configuration file contained "+").
      --  ??? Could be handled via a "*" star handle

      Indentation : Natural := 0;
      --  Current indentation for streams.

      TZ : Time_Offset := UTC_Time_Offset;
      --  Time zone cache, assuming that the OS will not change time zones
      --  while this partition is running.

      Activated : Boolean := False;
      --  Whether this package has been activated. It is activated when at
      --  least one config file is found (and parsed). Until it is activated,
      --  no trace_handle will ever log anything.
   end record;

   Global : Global_Vars;

   procedure Create_Exception_Handle (Handle : Trace_Handle);
   --  Create the exception handle associated with Handle.

   function Local_Sub_Second (T : Ada.Calendar.Time) return Integer;
   pragma Inline (Local_Sub_Second);
      --  Version of Local_Sub_Second taking advantage of the timezone cache

   function Find_Handle (Unit_Name_Upper_Case : String) return Trace_Handle;
   --  Return the debug handle associated with Unit_Name_Upper_Case,
   --  or null if there is none. The case of Unit_Name_Upper_Case is
   --  not changed.
   --  Note: this subprogram doesn't do any locking, it is the
   --  responsability of the called to make sure that not two tasks
   --  can access it at the same time.

   function Find_Wildcard_Handle
     (Unit_Name_Upper_Case : String) return Trace_Handle;
   --  Check whether there is a module name that contains a "*" and that can be
   --  used to provide the default configuration for Unit_Name_Upper_Case

   function Wildcard_Applies_To
     (Upper_Name : String; Upper_Star : String) return Boolean;
   --  Whether the module Upper_Name should take its default configuration from
   --  Upper_Wildcard_Name.

   function Find_Stream
     (Stream_Name      : String;
      Config_File_Name : String;
      Append           : Boolean) return Trace_Stream;
   --  Return the stream associated with that name (either an existing one or
   --  one created by a factory), or null if the default stream should be
   --  applied. This program doesn't do any locking, and must be called from
   --  withing appropriately locked code.

   procedure Log
     (Handle        : Trace_Handle;
      Message       : String;
      Location      : String := GNAT.Source_Info.Source_Location;
      Entity        : String := GNAT.Source_Info.Enclosing_Entity;
      Message_Color : String := Default_Fg);
   --  Log a message to Handle unconditionally

   procedure Put_Absolute_Time (Stream : in out Trace_Stream_Record'Class);
   --  Print the absolute time in Handle. No locking is done, this is the
   --  responsability of the caller. No colors is modified either.

   procedure Put_Elapsed_Time
     (Handle : in out Trace_Handle_Record'Class;
      Stream : in out Trace_Stream_Record'Class);
   --  Print the elapsed time the last call to Trace for this Handle. No
   --  locking done.

   procedure Put_Stack_Trace (Stream : in out Trace_Stream_Record'Class);
   --  Print the stack trace for this handle. No locking done

   function Config_File
     (Filename : Virtual_File;
      Default  : Virtual_File)
      return Virtual_File;
   --  Return the name of the config file to use.
   --  If Filename is specified, this is the file to use, providing it exists.
   --  Otherwise, we use a .gnatdebug in the current directory, and if there is
   --  none, Default if it exists.
   --  The empty string is returned if no such file was found.

   function Create_Internal
     (Unit_Name        : String;
      From_Config_File : Boolean;
      Default          : Default_Activation_Status := From_Config;
      Stream           : Trace_Stream := null;
      Factory          : Handle_Factory := null;
      Finalize         : Boolean := True) return Trace_Handle;
   --  Internal version of Create.

   function Get_Process_Id return Integer;
   --  Return the process ID of the current process
   pragma Import (C, Get_Process_Id, "getpid");

   type File_Type_Access is access all File_Type;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (File_Type, File_Type_Access);

   type File_Stream_Record is new Trace_Stream_Record with record
      File : File_Type_Access;
   end record;
   overriding procedure Put (Stream : in out File_Stream_Record; Str : String);
   overriding procedure Newline (Stream : in out File_Stream_Record);
   overriding procedure Close (Stream : in out File_Stream_Record);
   --  Logs to a file

   type Stdout_Stream_Record is new Trace_Stream_Record with null record;
   overriding procedure Put
     (Stream : in out Stdout_Stream_Record; Str : String);
   overriding procedure Newline (Stream : in out Stdout_Stream_Record);
   --  Logs to stdout

   type Stderr_Stream_Record is new Trace_Stream_Record with null record;
   overriding procedure Put
     (Stream : in out Stderr_Stream_Record; Str : String);
   overriding procedure Newline (Stream : in out Stderr_Stream_Record);
   --  Logs to stderr

   -----------------
   -- Find_Handle --
   -----------------

   function Find_Handle (Unit_Name_Upper_Case : String) return Trace_Handle is
      Tmp : Trace_Handle := Global.Handles_List;
   begin
      while Tmp /= null
        and then Tmp.Name.all /= Unit_Name_Upper_Case
      loop
         Tmp := Tmp.Next;
      end loop;
      return Tmp;
   end Find_Handle;

   -------------------------
   -- Wildcard_Applies_To --
   -------------------------

   function Wildcard_Applies_To
     (Upper_Name : String; Upper_Star : String) return Boolean
   is
   begin
      if Upper_Star (Upper_Star'First) = '*' then
         --  Test must include '.' in the suffix
         if Ends_With
           (Upper_Name, Upper_Star (Upper_Star'First + 1 .. Upper_Star'Last))
         then
            return True;
         end if;

      elsif Upper_Star (Upper_Star'Last) = '*' then
         --  "MODULE.*" should include "MODULE" itself
         if Upper_Name =
           Upper_Star (Upper_Star'First .. Upper_Star'Last - 2)
         then
            return True;
         end if;

         --  Otherwise "MODULE.*" should match "MODULE.FOO" but not
         --  MODULEFOO.BAR

         if Starts_With
           (Upper_Name, Upper_Star (Upper_Star'First .. Upper_Star'Last - 1))
         then
            return True;
         end if;
      end if;
      return False;
   end Wildcard_Applies_To;

   --------------------------
   -- Find_Wildcard_Handle --
   --------------------------

   function Find_Wildcard_Handle
     (Unit_Name_Upper_Case : String) return Trace_Handle
   is
      Tmp : Trace_Handle := Global.Wildcard_Handles_List;
   begin
      while Tmp /= null loop
         if Wildcard_Applies_To
           (Upper_Name => Unit_Name_Upper_Case,
            Upper_Star => Tmp.Name.all)
         then
            return Tmp;
         end if;

         Tmp := Tmp.Next;
      end loop;
      return null;
   end Find_Wildcard_Handle;

   ------------------------
   -- Show_Configuration --
   ------------------------

   procedure Show_Configuration (Output : Output_Proc) is
      Tmp : Trace_Handle := Global.Handles_List;
   begin
      while Tmp /= null loop
         if Tmp.Stream /= null then
            if Tmp.Active then
               Output (Tmp.Name.all & "=yes >" & Tmp.Stream.Name.all);
            else
               Output (Tmp.Name.all & "=no >" & Tmp.Stream.Name.all);
            end if;
         else
            if Tmp.Active then
               Output (Tmp.Name.all & "=yes");
            else
               Output (Tmp.Name.all & "=no");
            end if;
         end if;
         Tmp := Tmp.Next;
      end loop;
   end Show_Configuration;

   -----------------
   -- Find_Stream --
   -----------------

   function Find_Stream
     (Stream_Name      : String;
      Config_File_Name : String;
      Append           : Boolean) return Trace_Stream
   is
      procedure Add_To_Streams (Tmp : Trace_Stream);

      --------------------
      -- Add_To_Streams --
      --------------------

      procedure Add_To_Streams (Tmp : Trace_Stream) is
      begin
         --  If possible, do not put this first on the list of streams,
         --  since it would become the default stream
         if Global.Streams_List = null then
            Global.Streams_List := Tmp;
            Tmp.Next := null;
         else
            Tmp.Next := Global.Streams_List.Next;
            Global.Streams_List.Next := Tmp;
         end if;
      end Add_To_Streams;

      Name  : constant String := Trim (Stream_Name, Ada.Strings.Both);

      Tmp   : Trace_Stream;
      Colon : Natural;
      TmpF  : Stream_Factories_List;

   begin
      if Name = "" then
         return null;
      end if;

      Lock;

      --  Do we have a matching existing stream ?

      Tmp := Global.Streams_List;
      while Tmp /= null loop
         if Tmp.Name.all = Name then
            Unlock;
            return Tmp;
         end if;
         Tmp := Tmp.Next;
      end loop;

      Colon := Index (Name, ":");
      if Colon < Name'First then
         Colon := Name'Last + 1;
      end if;

      --  Do we have a matching factory (if we start with "&")?

      if Name = "&1" then
         Tmp := new Stdout_Stream_Record'
           (Name => new String'(Name),
            Next => null);
         Add_To_Streams (Tmp);

      elsif Name = "&2" then
         Tmp := new Stderr_Stream_Record'
           (Name => new String'(Name),
            Next => null);
         Add_To_Streams (Tmp);

      elsif Name (Name'First) = '&' then
         Tmp := null;
         TmpF := Global.Factories_List;

         while TmpF /= null loop
            if TmpF.Name.all = Name (Name'First .. Colon - 1) then
               if Colon < Name'Last then
                  Tmp := TmpF.Factory.New_Stream
                    (Name (Colon + 1 .. Name'Last));
               else
                  Tmp := TmpF.Factory.New_Stream ("");
               end if;

               Tmp.Name := new String'(Name);
               Add_To_Streams (Tmp);
               exit;
            end if;

            TmpF := TmpF.Next;
         end loop;

      else
         Tmp := new File_Stream_Record'
           (Name => new String'(Name),
            Next => null,
            File => new File_Type);

         declare
            Max_Date_Width : constant Natural := 10; --  "yyyy-mm-dd"
            Max_PID_Width  : constant Natural := 12;
            Max_Name_Last  : constant Natural :=
              Name'Last + Max_Date_Width + Max_PID_Width;
            Name_Tmp       : String (Name'First .. Max_Name_Last);
            Index          : Integer := Name_Tmp'First;
            N              : Integer := Name'First;
         begin
            while N <= Name'Last loop
               if Name (N) = '$' then
                  if N < Name'Last
                    and then Name (N + 1) = '$'
                  then
                     declare
                        Pid : constant String :=
                          Integer'Image (Get_Process_Id);
                     begin
                        Name_Tmp (Index .. Index + Pid'Length - 2) :=
                          Pid (Pid'First + 1 .. Pid'Last);
                        Index := Index + Pid'Length - 1;
                        N     := N + 1;
                     end;

                  elsif N < Name'Last
                    and then (Name (N + 1) = 'D' or else Name (N + 1) = 'T')
                  then
                     declare
                        Date : constant String :=
                          Image (Clock, ISO_Date
                                        & (if Name (N + 1) = 'T'
                                           then "T%T"
                                           else ""));
                     begin
                        Name_Tmp (Index .. Index + Date'Length - 1) := Date;
                        Index := Index + Date'Length;
                        N     := N + 1;
                     end;

                  else
                     Name_Tmp (Index) := Name (N);
                     Index := Index + 1;
                  end if;

               else
                  Name_Tmp (Index) := Name (N);
                  Index := Index + 1;
               end if;

               N := N + 1;
            end loop;

            declare
               N : constant String := Normalize_Pathname
                 (Name_Tmp (Name_Tmp'First .. Index - 1),
                  Dir_Name (Config_File_Name));
            begin
               if Append
                 and then Is_Regular_File (N)
               then
                  Open (File_Stream_Record (Tmp.all).File.all, Append_File, N);
               else
                  Create (File_Stream_Record (Tmp.all).File.all, Out_File, N);
               end if;

            exception
               when Ada.Text_IO.Use_Error =>
                  --  Default to stderr
                  Unchecked_Free (Tmp);
                  Tmp := new Stderr_Stream_Record'
                    (Name => new String'(Name), Next => null);
            end;

            Add_To_Streams (Tmp);
         end;
      end if;

      --  Else use the default stream

      Unlock;
      return Tmp;
   end Find_Stream;

   ------------
   -- Create --
   ------------

   function Create
     (Unit_Name : String;
      Default   : Default_Activation_Status := From_Config;
      Stream    : String := "";
      Factory   : Handle_Factory := null;
      Finalize  : Boolean := True) return Trace_Handle
   is
   begin
      if Debug_Mode then
         return Create_Internal
           (Unit_Name => Unit_Name,
            From_Config_File => False,
            Default   => Default,
            Stream    => Find_Stream (Stream, "", Append => False),
            Factory   => Factory,
            Finalize  => Finalize);
      else
         return null;
      end if;
   end Create;

   ---------------------
   -- Create_Internal --
   ---------------------

   function Create_Internal
     (Unit_Name        : String;
      From_Config_File : Boolean;
      Default          : Default_Activation_Status := From_Config;
      Stream           : Trace_Stream := null;
      Factory          : Handle_Factory := null;
      Finalize         : Boolean := True) return Trace_Handle
   is
      Tmp, Tmp2    : Trace_Handle    := null;
      Wildcard_Tmp : Trace_Handle    := null;
      Upper_Case   : constant String := To_Upper (Unit_Name);
      Is_Star      : Boolean;

   begin
      if Debug_Mode then
         Lock;

         Is_Star := Starts_With (Unit_Name, "*.")
           or else Ends_With (Unit_Name, ".*");

         if Is_Star then
            Tmp := Find_Wildcard_Handle (Upper_Case);
         else
            Tmp := Find_Handle (Upper_Case);
         end if;

         if Tmp = null then
            if Factory /= null then
               Tmp := Factory.all;
            end if;

            if Tmp = null then
               Tmp := new Trace_Handle_Record;
            end if;

            Tmp.Name          := new String'(Upper_Case);
            Tmp.Forced_Active := False;
            Tmp.Count         := 1;
            Tmp.Timer         := No_Time;
            Tmp.Finalize      := Finalize;

            if Is_Star then
               Wildcard_Tmp                 := null;
               Tmp.Next                     := Global.Wildcard_Handles_List;
               Global.Wildcard_Handles_List := Tmp;
            else
               Wildcard_Tmp        := Find_Wildcard_Handle (Upper_Case);
               Tmp.Next            := Global.Handles_List;
               Global.Handles_List := Tmp;
            end if;

            if Wildcard_Tmp /= null then
               Tmp.Active := Wildcard_Tmp.Active;
               Tmp.Forced_Active := True;

               --  Unless we specified an explicit stream, inherit it
               if Stream = null then
                  Tmp.Stream := Wildcard_Tmp.Stream;
               end if;

            else
               Tmp.Active := Global.Default_Activation;
               Tmp.Stream := null;
            end if;
         end if;

         if Stream /= null then
            --  Only override when we are parsing the configuration file, so
            --  that if we have the following:
            --      Me : Trace_Handle := Create ("ME", Stream => "str1");
            --      parse config file, which contains "ME=yes >str2"
            --      Me := Create ("ME", Stream => "str3")
            --  then "ME" is sent to "str2" (priority is given to the config
            --  file.

            if From_Config_File or else Tmp.Stream = null then
               Tmp.Stream := Stream;
            end if;
         end if;

         if not Tmp.Forced_Active or else From_Config_File then
            if Default = On then
               Tmp.Active := True;
               Tmp.Forced_Active := True;

            elsif Default = Off then
               Tmp.Active := False;
               Tmp.Forced_Active := True;
            end if;
         end if;

         --  If we are declaring a "wildcard" handle, we need to check
         --  whether any existing handle would match (which will in
         --  general be the case, since handles are declared at
         --  elaboration time and star handles in the config file).

         if Is_Star then
            Tmp2 := Global.Handles_List;
            while Tmp2 /= null loop
               if Wildcard_Applies_To
                 (Tmp2.Name.all, Upper_Star => Upper_Case)
               then
                  --  Always override the status of matching streams:
                  --  There are two scenarios here:
                  --     - in a given config file, we always respect the order
                  --  of declarations, thus wildcards should in general be put
                  --  at the beginning.
                  --     - if a wildcard is declared later on in Ada, we want
                  --  it to impact existing streams as well (as a convenience
                  --  for forcing specific settings from the code.
                  --
                  --  So do not check Tmp2.Forced_Active

                  Tmp2.Active := Tmp.Active;

                  if Tmp2.Stream = null then
                     Tmp2.Stream := Tmp.Stream;
                  end if;
               end if;

               Tmp2 := Tmp2.Next;
            end loop;
         end if;

         Unlock;
      end if;
      return Tmp;
   exception
      when others =>
         Unlock;
         raise;
   end Create_Internal;

   ------------------------
   -- Predefined handles --
   ------------------------
   --  This must be done after the body of Create has been seen

   Absolute_Time    : constant Trace_Handle := Create ("DEBUG.ABSOLUTE_TIME");
   Absolute_Date    : constant Trace_Handle :=
     Create ("DEBUG.ABSOLUTE_DATE", Off);
   Micro_Time       : constant Trace_Handle :=
      Create ("DEBUG.MICRO_TIME", Off);
   Elapsed_Time     : constant Trace_Handle := Create ("DEBUG.ELAPSED_TIME");
   Stack_Trace      : constant Trace_Handle := Create ("DEBUG.STACK_TRACE");
   Colors           : constant Trace_Handle := Create ("DEBUG.COLORS");
   Enclosing_Entity : constant Trace_Handle :=
     Create ("DEBUG.ENCLOSING_ENTITY");
   Location         : constant Trace_Handle := Create ("DEBUG.LOCATION");
   Count_Trace      : constant Trace_Handle := Create ("DEBUG.COUNT");
   Finalize_Traces  : constant Trace_Handle :=
     Create ("DEBUG.FINALIZE_TRACES", On);
   --  If set to Off, this module will not be finalized, and traces will still
   --  be activated when the program itself is finalized by GNAT

   ---------------
   -- Unit_Name --
   ---------------

   function Unit_Name (Handle : Trace_Handle) return String is
   begin
      return Handle.Name.all;
   end Unit_Name;

   -----------------------------
   -- Create_Exception_Handle --
   -----------------------------

   procedure Create_Exception_Handle (Handle : Trace_Handle) is
      Default : Default_Activation_Status;
   begin
      if Handle.Exception_Handle = null then
         --  Default activation should be the same as the handle
         if Handle.Active then
            Default := On;
         else
            Default := Off;
         end if;

         Handle.Exception_Handle := Create
           (Unit_Name => Handle.Name.all & ".EXCEPTIONS",
            Default   => Default);

         --  Unless the config file specified an explicit stream,
         --  we inherit the one from Handle.

         if Handle.Exception_Handle.Stream = null then
            Handle.Exception_Handle.Stream := Handle.Stream;
         end if;
      end if;
   end Create_Exception_Handle;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Handle : Trace_Handle;
      E      : Ada.Exceptions.Exception_Occurrence;
      Msg    : String := "Unexpected exception: ";
      Color  : String := Default_Fg) is
   begin
      Create_Exception_Handle (Handle);
      Trace (Handle.Exception_Handle,
             Msg & Ada.Exceptions.Exception_Information (E),
             Color => Color);
   end Trace;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Handle   : Trace_Handle;
      Message  : String;
      Color    : String := Default_Fg;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if Debug_Mode
        and then Global.Activated
        and then Global.Handles_List /= null  --  module not terminated
      then
         if Handle.Active then
            Log (Handle, Message, Location, Entity, Message_Color => Color);
         end if;

         if Count_Trace /= null then
            Count_Trace.Count := Count_Trace.Count + 1;
         end if;

         --  Always increment the count: that way, testsuites can easily count
         --  the number of queries that would have been emitted, even if they
         --  don't explicitly log.
         Handle.Count := Handle.Count + 1;
      end if;
   end Trace;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Handle             : Trace_Handle;
      Condition          : Boolean;
      Error_Message      : String;
      Message_If_Success : String := "";
      Raise_Exception    : Boolean := True;
      Location           : String := GNAT.Source_Info.Source_Location;
      Entity             : String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if Debug_Mode
        and then Global.Activated
        and then Global.Handles_List /= null
        and then Handle.Active
      then
         if not Condition then
            Create_Exception_Handle (Handle);
            Trace
              (Handle.Exception_Handle,
               Error_Message, Location, Entity, Red_Bg & Default_Fg);

            if Raise_Exception then
               Raise_Assert_Failure
                 (Error_Message & " (" & Entity & " at " &
                  Location & ")");
            end if;

         elsif Message_If_Success'Length /= 0 then
            Trace (Handle, Message_If_Success, Location, Entity);
         end if;
      end if;
   end Assert;

   ---------------------
   -- Increase_Indent --
   ---------------------

   procedure Increase_Indent
     (Handle : Trace_Handle := null; Msg : String := "")
   is
   begin
      if Handle /= null and then Msg /= "" then
         Trace (Handle, Msg);
      end if;
      Global.Indentation := Global.Indentation + 1;
   end Increase_Indent;

   ---------------------
   -- Decrease_Indent --
   ---------------------

   procedure Decrease_Indent
     (Handle : Trace_Handle := null; Msg : String := "") is
   begin
      if Global.Indentation > 0 then
         Global.Indentation := Global.Indentation - 1;
         if Handle /= null and then Msg /= "" then
            Trace (Handle, Msg);
         end if;

      else
         if Handle /= null then
            Trace (Handle, "Indentation error: two many decrease");
            if Msg /= "" then
               Trace (Handle, Msg);
            end if;
         end if;
      end if;
   end Decrease_Indent;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active (Handle : Trace_Handle; Active : Boolean) is
   begin
      Handle.Active := Active;
   end Set_Active;

   ------------
   -- Active --
   ------------

   function Active (Handle : Trace_Handle) return Boolean is
   begin
      if Global.Handles_List = null then
         --  If this module has been finalized, we always display the traces.
         --  These traces are generally when GNAT finalizes controlled types...
         return True;

      elsif Handle = null then
         --  In case Handle hasn't been initialized yet
         return False;

      else
         return Handle.Active;
      end if;
   end Active;

   ----------------------
   -- Local_Sub_Second --
   ----------------------

   function Local_Sub_Second (T : Ada.Calendar.Time) return Integer is
      Y  : Year_Number;
      M  : Month_Number;
      D  : Day_Number;
      H  : Ada.Calendar.Formatting.Hour_Number;
      Mi : Ada.Calendar.Formatting.Minute_Number;
      S  : Ada.Calendar.Formatting.Second_Number;
      Ss : Ada.Calendar.Formatting.Second_Duration;
      Ls : Boolean;
   begin
      Ada.Calendar.Formatting.Split (T, Y, M, D, H, Mi, S, Ss, Ls, Global.TZ);

      return Integer (Ss * 1000.0);
   end Local_Sub_Second;

   -----------------------
   -- Put_Absolute_Time --
   -----------------------

   procedure Put_Absolute_Time (Stream : in out Trace_Stream_Record'Class) is
      T  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Ms : constant String := Integer'Image (Local_Sub_Second (T));
   begin
      if Absolute_Date.Active then
         if Absolute_Time.Active then
            if Micro_Time.Active then
               Put (Stream, "(" & Image (T, ISO_Date & " %T:%e") & ')');
            else
               Put (Stream, "(" & Image (T, ISO_Date & " %T.")
                    & Ms (Ms'First + 1 .. Ms'Last) & ')');
            end if;
         else
            Put (Stream, "(" & Image (T, ISO_Date) & ')');
         end if;

      else
         if Micro_Time.Active then
            Put (Stream, "(" & Image (T, ISO_Date & " %T:%e") & ')');
         else
            Put (Stream, "(" & Image (T, "%T.")
                 & Ms (Ms'First + 1 .. Ms'Last) & ')');
         end if;
      end if;
   end Put_Absolute_Time;

   ----------------------
   -- Put_Elapsed_Time --
   ----------------------

   procedure Put_Elapsed_Time
     (Handle : in out Trace_Handle_Record'Class;
      Stream : in out Trace_Stream_Record'Class)
   is
      T   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Dur : Integer;
   begin
      if Handle.Timer /= No_Time then
         Dur := Integer ((T - Handle.Timer) * 1000);
         Put (Stream, "(elapsed:" & Integer'Image (Dur) & "ms)");
      end if;
      Handle.Timer := T;
   end Put_Elapsed_Time;

   ---------------------
   -- Put_Stack_Trace --
   ---------------------

   procedure Put_Stack_Trace (Stream : in out Trace_Stream_Record'Class) is
      Tracebacks : Tracebacks_Array (1 .. 50);
      Len        : Natural;
   begin
      Call_Chain (Tracebacks, Len);
      Put (Stream, "(callstack: ");
      for J in Tracebacks'First .. Len loop
         Put (Stream, System.Address_Image (PC_For (Tracebacks (J))) & ' ');
      end loop;
      Put (Stream, ")");
   end Put_Stack_Trace;

   -------------------
   -- Pre_Decorator --
   -------------------

   procedure Pre_Decorator
     (Handle  : in out Trace_Handle_Record;
      Stream  : in out Trace_Stream_Record'Class;
      Message : String)
   is
      pragma Unreferenced (Message);
   begin
      if Count_Trace.Active then
         declare
            C : constant String := Integer'Image (Count_Trace.Count);
            H : constant String := Integer'Image (Handle.Count);
         begin
            Put (Stream, H (H'First + 1 .. H'Last)
                 & '/' & C (C'First + 1 .. C'Last) & ' ');
         end;
      end if;
   end Pre_Decorator;

   --------------------
   -- Post_Decorator --
   --------------------

   procedure Post_Decorator
     (Handle   : in out Trace_Handle_Record;
      Stream   : in out Trace_Stream_Record'Class;
      Location : String;
      Entity   : String;
      Message  : String)
   is
      pragma Unreferenced (Message);

      Space_Inserted : Boolean := False;
      --  True when a space has been inserted after the main trace text, before
      --  the Post_Decorator information.

      procedure Ensure_Space;
      --  Insert a space if not done already

      ------------------
      -- Ensure_Space --
      ------------------

      procedure Ensure_Space is
      begin
         if not Space_Inserted then
            Put (Stream, " ");
            Space_Inserted := True;
         end if;
      end Ensure_Space;

   begin
      if (Absolute_Time.Active or else Absolute_Date.Active)
        and then Supports_Time (Stream)
      then
         Ensure_Space;
         Put_Absolute_Time (Stream);
      end if;

      if Elapsed_Time.Active then
         Ensure_Space;
         Put_Elapsed_Time (Handle, Stream);
      end if;

      if Traces.Location.Active then
         Ensure_Space;
         Put (Stream, "(loc: " & Location & ')');
      end if;

      if Enclosing_Entity.Active then
         Ensure_Space;
         Put (Stream, "(entity:" & Entity & ')');
      end if;

      if Stack_Trace.Active then
         Ensure_Space;
         Put_Stack_Trace (Stream);
      end if;
   end Post_Decorator;

   ---------
   -- Log --
   ---------

   procedure Log
     (Handle        : Trace_Handle;
      Message       : String;
      Location      : String := GNAT.Source_Info.Source_Location;
      Entity        : String := GNAT.Source_Info.Enclosing_Entity;
      Message_Color : String := Default_Fg)
   is
      Start, Last  : Natural;
      Continuation : constant String := '_' & Handle.Name.all & "_ ";
      Stream       : Trace_Stream;
      Color        : Boolean;
   begin
      if Message'Length = 0 then
         return;
      end if;

      if Handle.Stream /= null then
         Stream := Handle.Stream;
      else
         Stream := Global.Streams_List;
      end if;

      if Stream = null then
         return;
      end if;

      Color := Colors.Active and then Supports_Color (Stream.all);

      Lock;

      if Global.Indentation > 0 then
         Put (Stream.all, String'(1 .. Global.Indentation * 3 => ' '));
      end if;

      if Color then
         Put (Stream.all, Cyan_Fg);
      end if;

      Put (Stream.all, '[' & Handle.Name.all & "] ");
      Pre_Decorator (Handle.all, Stream.all, Message);

      if Color then
         Put (Stream.all, Message_Color);
      end if;

      Start := Message'First;
      loop
         Last := Start;
         while Last <= Message'Last
           and then Message (Last) /= ASCII.LF
           and then Message (Last) /= ASCII.CR
         loop
            Last := Last + 1;
         end loop;

         Put (Stream.all, Message (Start .. Last - 1));

         Start := Last + 1;
         exit when Start > Message'Last;

         Newline (Stream.all);
         if Color then
            Put (Stream.all, Purple_Fg & Default_Bg);
         end if;
         Put (Stream.all, Continuation);
         if Color then
            Put (Stream.all, Message_Color);
         end if;
      end loop;

      if Color then
         Put (Stream.all, Brown_Fg & Default_Bg);
      end if;

      Post_Decorator
        (Handle   => Handle.all,
         Stream   => Stream.all,
         Message  => Message,
         Location => Location,
         Entity   => Entity);

      if Color then
         Put (Stream.all, Default_Fg);
      end if;

      Newline (Stream.all);

      Unlock;

   exception
      when others =>
         Unlock;

         case On_Exception is
            when Propagate =>
               raise;
            when Ignore =>
               null;
            when Deactivate =>
               if Stream /= null then
                  begin
                     Close (Stream.all);
                  exception
                     when others =>
                        null;
                  end;
               end if;
         end case;
   end Log;

   -----------------
   -- Config_File --
   -----------------

   function Config_File
     (Filename : Virtual_File;
      Default  : Virtual_File)
      return Virtual_File
   is
      Env : GNAT.Strings.String_Access;
      Ret : Virtual_File;
   begin
      if Filename /= No_File and then Filename.Is_Regular_File then
         return Filename;
      end if;

      Env := Getenv (Config_File_Environment);

      --  First test the file described in the environment variable
      if Env /= null and then Env.all /= "" then
         Ret := Create (+Env.all);
         Free (Env);

         if Ret.Is_Regular_File then
            return Ret;
         end if;

         return No_File;
      end if;

      Free (Env);

      --  Then the file in the current directory

      Ret := Create_From_Dir (Get_Current_Dir, Default_Config_File);
      if Ret.Is_Regular_File then
         return Ret;
      end if;

      --  Then the file in the user's home directory
      Ret := Create_From_Dir (Get_Home_Directory, Default_Config_File);
      if Ret.Is_Regular_File then
         return Ret;
      end if;

      --  Finally the default file
      if Default /= No_File and then Is_Regular_File (Default) then
         return Default;
      end if;

      return No_File;
   end Config_File;

   -----------------------------
   -- Register_Stream_Factory --
   -----------------------------

   procedure Register_Stream_Factory
     (Name : String; Factory : Stream_Factory_Access)
   is
   begin
      Lock;
      Global.Factories_List := new Stream_Factories'
        (Name    => new String'("&" & Name),
         Factory => Factory,
         Next    => Global.Factories_List);
      Unlock;
   end Register_Stream_Factory;

   --------------------
   -- Supports_Color --
   --------------------

   function Supports_Color (Stream : Trace_Stream_Record) return Boolean is
      pragma Unreferenced (Stream);
   begin
      return True;
   end Supports_Color;

   -------------------
   -- Supports_Time --
   -------------------

   function Supports_Time (Stream : Trace_Stream_Record) return Boolean is
      pragma Unreferenced (Stream);
   begin
      return True;
   end Supports_Time;

   -----------
   -- Close --
   -----------

   procedure Close (Stream : in out Trace_Stream_Record) is
   begin
      Free (Stream.Name);
   end Close;

   ---------
   -- Put --
   ---------

   procedure Put (Stream : in out Stdout_Stream_Record; Str : String) is
      pragma Unreferenced (Stream);
   begin
      Put (Str);
   end Put;

   -------------
   -- Newline --
   -------------

   procedure Newline (Stream : in out Stdout_Stream_Record) is
      pragma Unreferenced (Stream);
   begin
      New_Line;
      Flush;
   end Newline;

   ---------
   -- Put --
   ---------

   procedure Put (Stream : in out Stderr_Stream_Record; Str : String) is
      pragma Unreferenced (Stream);
   begin
      Put (Ada.Text_IO.Standard_Error, Str);
   end Put;

   -------------
   -- Newline --
   -------------

   procedure Newline (Stream : in out Stderr_Stream_Record) is
      pragma Unreferenced (Stream);
   begin
      New_Line (Ada.Text_IO.Standard_Error);
      Flush (Ada.Text_IO.Standard_Error);
   end Newline;

   ---------
   -- Put --
   ---------

   procedure Put (Stream : in out File_Stream_Record; Str : String) is
   begin
      if Stream.File /= null then
         Put (Stream.File.all, Str);
      end if;
   end Put;

   -------------
   -- Newline --
   -------------

   procedure Newline (Stream : in out File_Stream_Record) is
   begin
      if Stream.File /= null then
         New_Line (Stream.File.all);
         Flush (Stream.File.all);
      end if;
   end Newline;

   -----------
   -- Close --
   -----------

   procedure Close (Stream : in out File_Stream_Record) is
   begin
      Close (Stream.File.all);
      Unchecked_Free (Stream.File);
      Close (Trace_Stream_Record (Stream));
   end Close;

   -----------------------
   -- Parse_Config_File --
   -----------------------

   procedure Parse_Config_File
     (Filename         : Virtual_File;
      Default          : Virtual_File := No_File;
      On_Exception     : On_Exception_Mode := Propagate;
      Force_Activation : Boolean := True)
   is
      File_Name : constant Virtual_File := Config_File (Filename, Default);

      Buffer            : Str_Access;
      File              : Mapped_File;
      Index, First, Max : Natural;
      Handle            : Trace_Handle;

      procedure Skip_Spaces (Skip_Newline : Boolean := True);
      --  Skip the spaces (including possibly newline), and leave Index on the
      --  first non blank character.

      procedure Skip_To_Newline (Stop_At_First_Blank : Boolean := False);
      --  Set Index after the last significant character on the line (either
      --  the ASCII.LF or after the last character in the buffer).

      -----------------
      -- Skip_Spaces --
      -----------------

      procedure Skip_Spaces (Skip_Newline : Boolean := True) is
      begin
         while Index <= Last (File)
           and then (Buffer (Index) = ' '
                     or else (Buffer (Index) = ASCII.LF
                              and then Skip_Newline)
                     or else Buffer (Index) = ASCII.CR
                     or else Buffer (Index) = ASCII.HT)
         loop
            Index := Index + 1;
         end loop;
      end Skip_Spaces;

      ---------------------
      -- Skip_To_Newline --
      ---------------------

      procedure Skip_To_Newline (Stop_At_First_Blank : Boolean := False) is
      begin
         while Index <= Last (File)
           and then Buffer (Index) /= ASCII.LF
           and then (not Stop_At_First_Blank
                     or else (Buffer (Index) /= ' '
                              and then Buffer (Index) /= ASCII.HT))
         loop
            Index := Index + 1;
         end loop;
      end Skip_To_Newline;

   begin
      GNATCOLL.Traces.On_Exception := On_Exception;

      if File_Name = No_File then
         if Force_Activation then
            Global.Activated := True;
         end if;

      else
         begin
            File := Open_Read (+File_Name.Full_Name);
         exception
            when Name_Error =>
               return;
         end;

         Lock;
         Global.Activated := True;
         Read (File);
         Buffer := Data (File);

         Index := 1;

         loop
            Skip_Spaces;
            exit when Index > Last (File);

            if Index + 1 <= Last (File)
              and then String (Buffer (Index .. Index + 1)) = "--"
            then
               Skip_To_Newline;

            else
               case Buffer (Index) is
                  when '>' =>
                     declare
                        Save   : Integer := Index + 1;
                        Stream : Trace_Stream;
                        Tmp    : Trace_Stream;
                        Append : constant Boolean := Buffer (Index + 1) = '>';
                     begin
                        if Append then
                           Save := Index + 2;
                        end if;

                        Skip_To_Newline;
                        if Buffer (Index - 1) = ASCII.CR then
                           Stream := Find_Stream
                             (String (Buffer (Save .. Index - 2)),
                              +File_Name.Full_Name,
                              Append);
                        else
                           Stream := Find_Stream
                             (String (Buffer (Save .. Index - 1)),
                              +File_Name.Full_Name,
                              Append);
                        end if;
                        if Stream /= null then
                           --  Put this first in the list, since that's the
                           --  default
                           if Global.Streams_List /= Stream then
                              Tmp := Global.Streams_List;
                              while Tmp /= null
                                and then Tmp.Next /= Stream
                              loop
                                 Tmp := Tmp.Next;
                              end loop;

                              if Tmp /= null then
                                 Tmp.Next := Stream.Next;
                                 Stream.Next := Global.Streams_List;
                                 Global.Streams_List := Stream;
                              end if;
                           end if;
                        end if;
                     end;

                  when '+' =>
                     Global.Default_Activation := True;
                     Skip_To_Newline;
                     Handle := Global.Handles_List;
                     while Handle /= null loop
                        if not Handle.Forced_Active
                          and then Handle /= Absolute_Time
                          and then Handle /= Elapsed_Time
                          and then Handle /= Stack_Trace
                          and then Handle /= Colors
                          and then Handle /= Enclosing_Entity
                          and then Handle /= Location
                        then
                           Handle.Active := True;

                           --  A later declaration of the stream in the code
                           --  should not be allowed to reset Active to False
                           Handle.Forced_Active := True;
                        end if;
                        Handle := Handle.Next;
                     end loop;

                  when others =>
                     First := Index;
                     while Index <= Last (File)
                       and then Buffer (Index) /= '='
                       and then Buffer (Index) /= '>'
                       and then Buffer (Index) /= '-'
                       and then Buffer (Index) /= ASCII.LF
                       and then Buffer (Index) /= ASCII.CR
                     loop
                        Index := Index + 1;
                     end loop;

                     Max := Index - 1;
                     while Max >= 1
                       and then (Buffer (Max) = ' '
                                 or else Buffer (Max) = ASCII.HT)
                     loop
                        Max := Max - 1;
                     end loop;

                     declare
                        Active : Default_Activation_Status := From_Config;
                        Stream : Trace_Stream := null;
                     begin
                        --  Is this active ?

                        if Index > Last (File)
                          or else Buffer (Index) /= '='
                        then
                           Active := On;
                        else
                           Index := Index + 1;
                           Skip_Spaces;
                           if Index + 1 > Last (File) or else
                             String (Buffer (Index .. Index + 1)) /= "no"
                           then
                              Active := On;
                           else
                              Active := Off;
                           end if;
                        end if;

                        --  What stream is this sent to ?

                        while Index <= Last (File)
                          and then Buffer (Index) /= '>'
                          and then Buffer (Index) /= ASCII.LF
                          and then Buffer (Index) /= ASCII.CR
                        loop
                           Index := Index + 1;
                        end loop;

                        if Index <= Last (File)
                          and then Buffer (Index) = '>'
                        then
                           declare
                              Save : Integer := Index + 1;
                              Append : constant Boolean :=
                                Buffer (Index + 1) = '>';
                           begin
                              if Append then
                                 Save := Index + 2;
                              end if;

                              Skip_To_Newline;

                              if Buffer (Index - 1) = ASCII.CR then
                                 Stream := Find_Stream
                                   (String (Buffer (Save .. Index - 2)),
                                    +File_Name.Full_Name, Append);
                              else
                                 Stream := Find_Stream
                                   (String (Buffer (Save .. Index - 1)),
                                    +File_Name.Full_Name, Append);
                              end if;
                           end;
                        else
                           Skip_To_Newline;
                        end if;

                        Handle := Create_Internal
                          (String (Buffer (First .. Max)),
                           From_Config_File => True,
                           Default => Active,
                           Stream => Stream);
                     end;
               end case;
            end if;
         end loop;

         Close (File);
         Unlock;
      end if;

   exception
      when others =>
         Unlock;
         raise;
   end Parse_Config_File;

   -----------------------
   -- Parse_Config_File --
   -----------------------

   procedure Parse_Config_File
     (Filename         : String := "";
      Default          : String := "";
      On_Exception     : On_Exception_Mode := Propagate;
      Force_Activation : Boolean := True)
   is
      F_Filename : Virtual_File;
      F_Default  : Virtual_File;
   begin
      if Filename = "" then
         F_Filename := No_File;
      else
         F_Filename := Create_From_Base (+Filename);
      end if;

      if Default = "" then
         F_Default := No_File;
      else
         F_Default := Create_From_Base (+Default);
      end if;

      Parse_Config_File
        (F_Filename, F_Default, On_Exception, Force_Activation);
   end Parse_Config_File;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      Tmp   : Trace_Handle;
      Next  : Trace_Handle;
      TmpS  : Trace_Stream;
      NextS : Trace_Stream;
      TmpF  : Stream_Factories_List;
      NextF : Stream_Factories_List;
   begin
      if Active (Finalize_Traces) then
         Lock;
         Tmp := Global.Handles_List;
         while Tmp /= null loop
            Next := Tmp.Next;

            if Tmp.Finalize then
               Free (Tmp.Name);
               Unchecked_Free (Tmp);
            end if;

            Tmp := Next;
         end loop;
         Global.Handles_List := null;

         Tmp := Global.Wildcard_Handles_List;
         while Tmp /= null loop
            Next := Tmp.Next;

            if Tmp.Finalize then
               Free (Tmp.Name);
               Unchecked_Free (Tmp);
            end if;

            Tmp := Next;
         end loop;
         Global.Wildcard_Handles_List := null;

         TmpS := Global.Streams_List;
         while TmpS /= null loop
            NextS := TmpS.Next;
            Close (TmpS.all);
            Unchecked_Free (TmpS);
            TmpS := NextS;
         end loop;
         Global.Streams_List := null;

         TmpF := Global.Factories_List;
         while TmpF /= null loop
            NextF := TmpF.Next;
            Free (TmpF.Name);
            Unchecked_Free (TmpF.Factory);
            Unchecked_Free (TmpF);
            TmpF := NextF;
         end loop;
         Global.Factories_List := null;

         Unlock;
      end if;
   end Finalize;

   ------------------------
   -- Set_Default_Stream --
   ------------------------

   procedure Set_Default_Stream (Name : String) is
      S : Trace_Stream;
      T : Trace_Stream;

   begin
      if Name'Length > 2
        and then Name (Name'First .. Name'First + 1) = ">>"
      then
         S := Find_Stream
           (Name (Name'First + 2 .. Name'Last), "", Append => True);
      else
         S := Find_Stream (Name, "", Append => False);
      end if;

      --  Put it first in the list

      if Global.Streams_List /= S then
         T := Global.Streams_List;
         while T.Next /= S loop
            T := T.Next;
         end loop;

         T.Next := S.Next;
         S.Next := Global.Streams_List;
         Global.Streams_List := S;
      end if;
   end Set_Default_Stream;

   -----------
   -- Count --
   -----------

   function Count (Handler : Trace_Handle) return Natural is
   begin
      if Handler = null then
         return 0;
      else
         return Handler.Count;
      end if;
   end Count;

begin
   --  This is the default stream, always register it
   Set_Default_Stream ("&1");
end GNATCOLL.Traces;
