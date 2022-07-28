------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2001-2022, AdaCore                     --
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
with Ada.Environment_Variables;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;

with Ada.Exceptions.Traceback;  use Ada.Exceptions.Traceback;
with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Traceback;            use GNAT.Traceback;

with Interfaces.C_Streams;      use Interfaces.C_Streams;
with System.Address_Image;
with System.Assertions;         use System.Assertions;

with GNATCOLL.Memory;
with GNATCOLL.Mmap;             use GNATCOLL.Mmap;
with GNATCOLL.Templates;
with GNATCOLL.Utils;            use GNATCOLL.Utils;

package body GNATCOLL.Traces is

   use type FILEs, size_t;
   use type GNATCOLL.Terminal.ANSI_Color, GNATCOLL.Terminal.ANSI_Style;
   use type GNATCOLL.Terminal.Full_Style;

   Max_Active_Decorators : constant := 40;
   --  Maximum number of active iterators

   On_Exception : On_Exception_Mode := Propagate;
   --  The behavior that should be adopted when something unexpected prevent
   --  the log stream to be written.

   --  Note: rev 1.5 of this file has a (disabled) support for symbolic
   --  tracebacks.

   --  ??? We could display the stack pointer with
   --  procedure Print_Sp is
   --     start : aliased Integer;
   --  begin
   --     Put_Line (System.Address_Image (Start'Address));
   --  end;

   A_Zero : aliased constant String := "a" & ASCII.NUL;
   W_Zero : aliased constant String := "w" & ASCII.NUL;

   Reset_All  : constant String := GNATCOLL.Terminal.Get_ANSI_Sequence
      ((Style => GNATCOLL.Terminal.Reset_All,
        Fg    => GNATCOLL.Terminal.Unchanged,
        Bg    => GNATCOLL.Terminal.Unchanged));

   Default_Style : constant Message_Style :=
      (Fg    => GNATCOLL.Terminal.Reset,
       Bg    => GNATCOLL.Terminal.Unchanged,
       Style => GNATCOLL.Terminal.Unchanged);
   --  The default style used for handles. This uses the terminal's
   --  default foreground.

   Default_Exception_Style : constant Message_Style :=
      (Fg    => GNATCOLL.Terminal.Black,
       Bg    => GNATCOLL.Terminal.Red,
       Style => GNATCOLL.Terminal.Unchanged);
   --  Highlight with a red background.
   --  This is used to report unexpected exceptions when an exception
   --  occurrence is passed to Trace.

   type Decorator_Array is
      array (1 .. Max_Active_Decorators) of Trace_Decorator;

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

      Active_Decorators : Decorator_Array;
      Active_Last       : Natural := 0;
      --  Never null after parsing the config file.
      --  Decorators on this list are all active, and also stored in the
      --  Handles_List.

      Wildcard_Handles_List : Trace_Handle := null;
      --  Contains the configuration for module names containing stars, for
      --  instance "*.EXCEPTIONS".

      Streams_List : Trace_Stream := null;
      --  The global list of all streams.
      --  The default stream is the first in the list.

      Factories_List : Stream_Factories_List := null;
      --  The global list of all factories.

      TZ : Time_Offset := UTC_Time_Offset;
      --  Time zone cache, assuming that the OS will not change time zones
      --  while this partition is running.

      Lock : aliased Atomic_Counter := 0;
      pragma Atomic (Lock);

      Absolute_Time    : Trace_Decorator;
      Absolute_Date    : Trace_Decorator;
      Micro_Time       : Trace_Decorator;
      Colors           : Trace_Decorator;
      Enclosing_Entity : Trace_Decorator;
      Location         : Trace_Decorator;
      Finalize_Traces  : Trace_Decorator;
      Split_Lines      : Trace_Decorator;
      --  The predefined decorators.
      --  ??? These are also stored in the lists above, so we might not need
      --  them.

      Default_Activation : Boolean := False;
      --  Default activation status for debug handles (ie whether the
      --  configuration file contained "+").
      --  ??? Could be handled via a "*" star handle

      Finalized   : Boolean := False;
      --  Whether the package has been finalized.
      --  When this is true, some trace_handles will have been freed, and it
      --  is therefore illegal to access them.
   end record;

   Global : Global_Vars;

   type Elapse_Time_Trace is new Trace_Decorator_Record with null record;
   overriding procedure After_Message
      (Self   : in out Elapse_Time_Trace;
       Handle : not null Logger;
       Msg    : in out Msg_Strings.XString);

   type Stack_Trace is new Trace_Decorator_Record with null record;
   overriding procedure After_Message
      (Self   : in out Stack_Trace;
       Handle : not null Logger;
       Msg    : in out Msg_Strings.XString);

   type Count_Trace is new Trace_Decorator_Record with null record;
   overriding procedure Before_Message
      (Self   : in out Count_Trace;
       Handle : not null Logger;
       Msg    : in out Msg_Strings.XString);

   type Memory_Trace is new Trace_Decorator_Record with record
      Previous   : GNATCOLL.Memory.Byte_Count := 0;
   end record;
   overriding procedure After_Message
      (Self   : in out Memory_Trace;
       Handle : not null Logger;
       Msg    : in out Msg_Strings.XString);

   type Ada_Memory_Trace is new Trace_Decorator_Record with record
      Previous : GNATCOLL.Memory.Byte_Count := 0;
   end record;
   overriding procedure After_Message
      (Self   : in out Ada_Memory_Trace;
       Handle : not null Logger;
       Msg    : in out Msg_Strings.XString);

   procedure Lock (The_Lock : aliased in out Atomic_Counter)
      with Inline_Always;
   procedure Unlock (The_Lock : aliased in out Atomic_Counter)
      with Inline_Always;
   --  For critical regions. A thread already owning the lock cannot try to
   --  take the lock again, or it will block.

   procedure Create_Exception_Handle (Handle : not null Trace_Handle);
   --  Create the exception handle associated with Handle.

   function Local_Sub_Second (T : Ada.Calendar.Time) return Integer;
   pragma Inline (Local_Sub_Second);
      --  Version of Local_Sub_Second taking advantage of the timezone cache
      --  return values in range 0 .. 999

   function Find_Handle
     (Handle : Trace_Handle; Name_Upper_Case : String) return Trace_Handle;
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
      Relative_Path_To : Virtual_File;
      Append           : Boolean) return Trace_Stream;
   --  Return the stream associated with that name (either an existing one or
   --  one created by a factory), or null if the default stream should be
   --  applied.
   --  The Stream_Name might include the settings for the stream, as in:
   --      "file.txt:buffer_size=0,async=true"

   procedure Put_Absolute_Time (Msg : in out Msg_Strings.XString);
   --  Print the absolute time in Handle. No locking is done, this is the
   --  responsability of the caller. No colors is modified either.

   function Config_File
     (Filename : Virtual_File;
      Default  : Virtual_File)
      return Virtual_File;
   --  Return the name of the config file to use.
   --  If Filename is specified, this is the file to use, providing it exists.
   --  Otherwise, we use a .gnatdebug in the current directory, and if there is
   --  none, Default if it exists.
   --  The empty string is returned if no such file was found.

   procedure Register_Handle
      (Handle           : not null Trace_Handle;
       Upper_Case       : String;
       Finalize         : Boolean := True);
   --  add Handle to the internal list and set default fields

   function Create_Internal
     (Unit_Name : String;
      Default   : Default_Activation_Status := From_Config;
      Stream    : Trace_Stream;
      Factory   : Handle_Factory := null;
      Finalize  : Boolean := True;
      Style     : Message_Style;
      From_Config_File : Boolean) return Trace_Handle;
   --  Internal version of Create

   function Get_Process_Id return Integer;
   --  Return the process ID of the current process
   pragma Import (C, Get_Process_Id, "getpid");

   type File_Stream_Record is new Trace_Stream_Record with record
      File           : FILEs := NULL_Stream;
      Lock           : aliased GNATCOLL.Atomic.Atomic_Counter := 0;
      Colors_Support : Boolean;

      File_Name      : GNAT.Strings.String_Access;
      --  The absolute path of the file associated to this stream, if any.
   end record;
   overriding procedure Put
      (Stream     : in out File_Stream_Record;
       Str        : Msg_Strings.XString);
   overriding procedure Close (Stream : in out File_Stream_Record);
   overriding function Supports_Color
      (Self : File_Stream_Record) return Boolean
      is (Self.Colors_Support);
   --  Logs to a file

   procedure Cache_Settings (Handle : not null Trace_Handle);
   --  Cache various settings in Handle, to avoid dispatching calls in Log
   --  and thus speed things up.
   --  These settings are changed much less frequently.

   ----------
   -- Lock --
   ----------

   procedure Lock (The_Lock : aliased in out Atomic_Counter) is
   begin
      while True loop
         --  In this package, the lock is owned during the time it takes
         --  to Put a string to a stream (async streams go even faster).
         --  It doesn't seem worth adding a "delay" in this loop, though
         --  the standard implementation would be to have a delay on a
         --  random number, and increase the delay every time we have to
         --  loop until a given maximum.

         while The_Lock /= 0 loop
            null;
         end loop;

         exit when Sync_Add_And_Fetch
            (The_Lock'Unchecked_Access, 1) = 1;
      end loop;
   end Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (The_Lock : aliased in out Atomic_Counter) is
   begin
      The_Lock := 0;
   end Unlock;

   -----------------
   -- Find_Handle --
   -----------------

   function Find_Handle
     (Handle : Trace_Handle; Name_Upper_Case : String) return Trace_Handle
   is
      Tmp : Trace_Handle := Handle;
   begin
      while Tmp /= null
        and then Tmp.Name.all /= Name_Upper_Case
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

      function Stream_Name return String;
      --  Return the name of the stream if there is one

      function Stream_Name return String is
      begin
         if Tmp.Stream /= null
            and then Tmp.Stream /= Global.Streams_List
         then
            return " >" & Tmp.Stream.Name.all;
         else
            return "";
         end if;
      end Stream_Name;

   begin
      if Global.Streams_List /= null then
         Output ("> " & Global.Streams_List.Name.all);
      end if;

      while Tmp /= null loop
         if Tmp.Active then
            Output (Tmp.Name.all & "=yes" & Stream_Name);
         elsif Tmp.all not in Trace_Decorator_Record'Class then
            --  Only output decorators when they are active
            Output (Tmp.Name.all & "=no" & Stream_Name);
         end if;
         Tmp := Tmp.Next;
      end loop;
   end Show_Configuration;

   -----------------
   -- Find_Stream --
   -----------------

   function Find_Stream
     (Stream_Name      : String;
      Relative_Path_To : Virtual_File;
      Append           : Boolean) return Trace_Stream
   is
      procedure Add_To_Streams (Tmp : Trace_Stream);

      --------------------
      -- Add_To_Streams --
      --------------------

      procedure Add_To_Streams (Tmp : Trace_Stream) is
      begin
         --  ??? Could use atomic operations to manipulate the
         --  list directly.
         Lock (Global.Lock);

         --  If possible, do not put this first on the list of streams,
         --  since it would become the default stream
         if Global.Streams_List = null then
            Global.Streams_List := Tmp;
            Tmp.Next := null;
         else
            Tmp.Next := Global.Streams_List.Next;
            Global.Streams_List.Next := Tmp;
         end if;

         Unlock (Global.Lock);
      end Add_To_Streams;

      Name  : constant String := Trim (Stream_Name, Ada.Strings.Both);

      Tmp   : Trace_Stream;
      Colon : Natural;
      TmpF  : Stream_Factories_List;
      Default_Colors : GNATCOLL.Terminal.Supports_Color :=
         GNATCOLL.Terminal.Auto;
      Term  : GNATCOLL.Terminal.Terminal_Info;

      Supports_Buffer : Boolean := True;
      Buf_Size : size_t := 1; -- Line buffering by default

   begin
      if Name = "" then
         return null;
      end if;

      --  Do we have a matching existing stream?
      --  Since we use a linked list and never remove elements from
      --  the list, we do not need locking.

      Tmp := Global.Streams_List;
      while Tmp /= null loop
         if Tmp.Name.all = Name then
            return Tmp;
         end if;
         Tmp := Tmp.Next;
      end loop;

      --  Parse stream options

      Colon := Index (Name, ":");
      if Colon < Name'First then
         Colon := Name'Last + 1;
      end if;

      declare
         Args  : String_List_Access := Split (Name, ':');
      begin
         for A of Args (Args'First + 1 .. Args'Last) loop
            if Starts_With (A.all, "buffer_size=") then
               begin
                  Buf_Size := size_t'Value (A (A'First + 12 .. A'Last));
               exception
                  when Constraint_Error =>
                     --  Ignore not numeric buffer_size value and Buf_Size
                     --  remains default.
                     null;
               end;

            elsif Starts_With (A.all, "colors=") then
               declare
                  V : constant String := To_Lower (A (A'First + 7 .. A'Last));
               begin
                  if V = "on" or else V = "true" then
                     Default_Colors := GNATCOLL.Terminal.Yes;
                  elsif V = "off" or else V = "false" then
                     Default_Colors := GNATCOLL.Terminal.No;
                  else
                     Default_Colors := GNATCOLL.Terminal.Auto;
                  end if;
               end;
            end if;
         end loop;

         Free (Args);
      end;

      --  Do we have a matching factory (if we start with "&")?

      if Name (Name'First .. Colon - 1) = "&1" then
         Term.Init_For_Stdout (Colors => Default_Colors);
         Tmp := new File_Stream_Record'
           (Name           => new String'(Name),
            File           => stdout,
            Colors_Support => Term.Has_ANSI_Colors,
            others         => <>);
         Add_To_Streams (Tmp);
         Supports_Buffer := False;

      elsif Name (Name'First .. Colon - 1) = "&2" then
         Term.Init_For_Stderr (Colors => Default_Colors);
         Tmp := new File_Stream_Record'
           (Name           => new String'(Name),
            File           => stderr,
            Colors_Support => Term.Has_ANSI_Colors,
            others         => <>);
         Add_To_Streams (Tmp);
         Supports_Buffer := False;

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
         declare
            use GNATCOLL.Templates;

            Now : constant Ada.Calendar.Time := Clock;

            Nam_Dollar : aliased String := "$";
            Val_Dollar : aliased String :=
              Trim (Get_Process_Id'Img, Ada.Strings.Both);

            Nam_D      : aliased String := "D";
            Val_D      : aliased String := Image (Now, ISO_Date);

            Nam_T      : aliased String := "T";
            Val_T      : aliased String := Val_D & Image (Now, "T%H%M%S");

            Predef_Substitutions : constant Substitution_Array :=
              ((Name  => Nam_Dollar'Unchecked_Access,
                Value => Val_Dollar'Unchecked_Access),

               (Name  => Nam_D'Unchecked_Access,
                Value => Val_D'Unchecked_Access),

               (Name  => Nam_T'Unchecked_Access,
                Value => Val_T'Unchecked_Access));

            function Substitute_Cb
              (Var : String; Quoted : Boolean) return String;
            --  Callback for variable substitution in Name

            --------------------
            --  Substitute_Cb --
            --------------------

            function Substitute_Cb
              (Var : String; Quoted : Boolean) return String
            is
               pragma Unreferenced (Quoted);
               --  No way to "use Ada.Environment_Variables;" because of
               --  visibility conflict with Traces.Exists and GNAT can't
               --  discover it, see R924-001.
            begin
               if Ada.Environment_Variables.Exists (Var) then
                  return Ada.Environment_Variables.Value (Var);
               else
                  raise Invalid_Substitution;
               end if;
            end Substitute_Cb;

            N : constant String := Normalize_Pathname
              (Substitute
                 (Str        => Name (Name'First .. Colon - 1),
                  Substrings => Predef_Substitutions,
                  Callback   => Substitute_Cb'Unrestricted_Access,
                  Delimiter  => '$'),
               +Relative_Path_To.Full_Name.all);
            N_Zero : aliased constant String := N & ASCII.NUL;
            F      : FILEs;
         begin
            if Append then
               F := fopen (N_Zero'Address, mode => A_Zero'Address);
            else
               F := fopen (N_Zero'Address, mode => W_Zero'Address);
            end if;

            if F = NULL_Stream then
               F := stderr;
            end if;

            Term.Init_For_File (Colors => Default_Colors);

            Tmp := new File_Stream_Record'
              (Name           => new String'(Name),
               File           => F,
               Colors_Support => Term.Has_ANSI_Colors,
               File_Name      => new String'(N),
               others         => <>);
            Add_To_Streams (Tmp);
         end;
      end if;

      if Tmp /= null
         and then Tmp.all in File_Stream_Record'Class
         and then Supports_Buffer
      then
         declare
            Dummy : int;
         begin
            Dummy := setvbuf
              (File_Stream_Record (Tmp.all).File,
               System.Null_Address,
               (case Buf_Size is
                   when 0      => IONBF,  -- unbuffered
                   when 1      => IOLBF,  -- line buffered
                   when others => IOFBF), -- full buffered
               Buf_Size);
         end;
      end if;

      --  Else use the default stream

      return Tmp;
   end Find_Stream;

   ---------------------
   -- Get_Stream_File --
   ---------------------

   function Get_Stream_File
     (Handle : not null access Trace_Handle_Record'Class) return Virtual_File
   is
   begin
      if Handle.Stream /= null
        and then Handle.Stream.all in File_Stream_Record'Class
        and then File_Stream_Record (Handle.Stream.all).File_Name /= null
      then
         return Create
           (+File_Stream_Record (Handle.Stream.all).File_Name.all);
      end if;

      return No_File;
   end Get_Stream_File;

   ------------
   -- Create --
   ------------

   function Create
     (Unit_Name : String;
      Default   : Default_Activation_Status := From_Config;
      Stream    : String := "";
      Factory   : Handle_Factory := null;
      Finalize  : Boolean := True) return Trace_Handle is
   begin
      return Create_Internal
         (From_Config_File => False,
          Unit_Name        => Unit_Name,
          Default          => Default,
          Stream           => Find_Stream (Stream, No_File, Append => False),
          Factory          => Factory,
          Style            => Default_Style,
          Finalize         => Finalize);
   end Create;

   ---------------------
   -- Create_Internal --
   ---------------------

   function Create_Internal
     (Unit_Name : String;
      Default   : Default_Activation_Status := From_Config;
      Stream    : Trace_Stream;
      Factory   : Handle_Factory := null;
      Finalize  : Boolean := True;
      Style     : Message_Style;
      From_Config_File : Boolean) return Trace_Handle
   is
      Is_Star    : constant Boolean := Starts_With (Unit_Name, "*.")
        or else Ends_With (Unit_Name, ".*");
      Handle     : Trace_Handle;
      Upper_Case : constant String := To_Upper (Unit_Name);
      Tmp2     : Trace_Handle;
      Wildcard : Trace_Handle;
   begin
      --  Do we already have an existing handle ?

      Handle := Find_Handle
        ((if Is_Star then Global.Wildcard_Handles_List
          else Global.Handles_List),
         Upper_Case);

      if Handle = null then
         if Factory /= null then
            Handle := Factory.all;
         end if;

         if Handle = null then
            Handle := new Trace_Handle_Record;
         end if;

         Register_Handle
           (Handle     => Handle,
            Upper_Case => Upper_Case,
            Finalize   => Finalize);

         --  Unless both settings are already known, check if we have a
         --  wildcard.

         if (Default = From_Config or else Stream = null)
           and then not Is_Star
         then
            Wildcard := Find_Wildcard_Handle (Handle.Name.all);

            if Wildcard /= null then
               Set_Active (Handle, Wildcard.Active);
               Handle.Forced_Active := True;

               --  Unless we specified an explicit stream, inherit it
               if Stream = null and then Wildcard.Stream /= null then
                  Handle.Stream := Wildcard.Stream;
                  Handle.Stream_Is_Default := Wildcard.Stream_Is_Default;
               end if;

            else
               Set_Active (Handle, Global.Default_Activation);
            end if;
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

         if From_Config_File or else Handle.Stream_Is_Default then
            Handle.Stream := Stream;
            Handle.Stream_Is_Default := False;
         end if;

      --  A wildcard only impacts the stream of loggers if it has its own
      --  stream.

      elsif not Is_Star then
         --  Use the default stream. If we are still parsing the config
         --  file, we might not have this info yet, so we set Stream to
         --  'null' and it will be overridden later
         if not From_Config_File and then Handle.Stream_Is_Default then
            Handle.Stream := Global.Streams_List;
            Handle.Stream_Is_Default := True;
         end if;
      end if;

      --  Set activation

      if not Handle.Forced_Active or else From_Config_File then
         case Default is
            when On =>
               Handle.Forced_Active := True;
               Set_Active (Handle, Active => True);
            when Off =>
               Handle.Forced_Active := True;
               Set_Active (Handle, Active => False);
            when From_Config =>
               null;
         end case;
      end if;

      --  If we are declaring a "wildcard" handle, we need to check
      --  whether any existing handle would match (which will in
      --  general be the case, since handles are declared at
      --  elaboration time and star handles in the config file).

      if Is_Star then
         Tmp2 := Global.Handles_List;
         while Tmp2 /= null loop
            if Wildcard_Applies_To
              (Tmp2.Name.all, Upper_Star => Handle.Name.all)
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

               Set_Active (Tmp2, Handle.Active);

               if Style /= Use_Default_Style
                  and then Tmp2.Default_Style = Default_Style
               then
                  Tmp2.Default_Style := Style;
               end if;

               if Tmp2.Stream_Is_Default and then Handle.Stream /= null then
                  Tmp2.Stream := Handle.Stream;
                  Tmp2.Stream_Is_Default := Handle.Stream_Is_Default;
               end if;
            end if;

            Tmp2 := Tmp2.Next;
         end loop;
      end if;

      if Style /= Use_Default_Style then
         Handle.Default_Style := Style;
      end if;

      Cache_Settings (Handle);

      return Handle;
   end Create_Internal;

   ------------
   -- Exists --
   ------------

   function Exists (Unit_Name : String) return Boolean is
      Is_Star    : constant Boolean := Starts_With (Unit_Name, "*.")
        or else Ends_With (Unit_Name, ".*");
      Handle     : Trace_Handle;
      Upper_Case : constant String := To_Upper (Unit_Name);
   begin
      Handle := Find_Handle
        ((if Is_Star then Global.Wildcard_Handles_List
          else Global.Handles_List),
         Upper_Case);

      return Handle /= null;
   end Exists;

   --------------------
   -- Cache_Settings --
   --------------------

   procedure Cache_Settings (Handle : not null Trace_Handle) is
   begin
      --  If we have already registered the default decorators

      if Global.Colors /= null
         and then Handle.Stream /= null
      then
         Handle.With_Time :=
            (Global.Absolute_Time.Active or else Global.Absolute_Date.Active)
            and then Handle.Stream.Supports_Time;
         Handle.With_Colors :=
            Global.Colors.Active and then Handle.Stream.Supports_Color;
      end if;
   end Cache_Settings;

   ---------------------
   -- Register_Handle --
   ---------------------

   procedure Register_Handle
      (Handle           : not null Trace_Handle;
       Upper_Case       : String;
       Finalize         : Boolean := True)
   is
      Is_Star : constant Boolean :=
         Starts_With (Upper_Case, "*.")
         or else Ends_With (Upper_Case, ".*");
   begin
      Handle.Name              := new String'(Upper_Case);
      Handle.Forced_Active     := False;
      Handle.Count             := 0;
      Handle.Timer             := No_Time;
      Handle.Finalize          := Finalize;
      Handle.Active            := False;
      Handle.Stream_Is_Default := True;

      Lock (Global.Lock);

      if Is_Star then
         Handle.Next                  := Global.Wildcard_Handles_List;
         Global.Wildcard_Handles_List := Handle;
      else
         Handle.Next         := Global.Handles_List;
         Global.Handles_List := Handle;
      end if;

      Unlock (Global.Lock);
   exception
      when others =>
         Unlock (Global.Lock);
         raise;
   end Register_Handle;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
      (Handle : not null access Trace_Handle_Record'Class; Active : Boolean)
   is
      Tmp : Trace_Handle;
      Dec : Trace_Decorator;
   begin
      Handle.Active := Active;

      if Handle.all in Trace_Decorator_Record'Class then
         Dec := Trace_Decorator (Handle);

         if Dec /= Global.Colors
            and then Dec /= Global.Finalize_Traces
            and then Dec /= Global.Split_Lines
         then
            --  If active, store it in the list of active decorators
            if Active then
               for A in 1 .. Global.Active_Last loop
                  if Global.Active_Decorators (A) = Dec then
                     --  Already in the list
                     return;
                  end if;
               end loop;

               --  ??? Should check if we have too many decorators
               Global.Active_Last := Global.Active_Last + 1;
               Global.Active_Decorators (Global.Active_Last) := Dec;

            else
               for A in 1 .. Global.Active_Last loop
                  if Global.Active_Decorators (A) = Dec then
                     Global.Active_Decorators (A .. Global.Active_Last - 1) :=
                        Global.Active_Decorators (A + 1 .. Global.Active_Last);
                     Global.Active_Last := Global.Active_Last - 1;
                     exit;
                  end if;
               end loop;
            end if;
         end if;

         if Dec = Global.Colors
            or else Dec = Global.Absolute_Time
            or else Dec = Global.Absolute_Date
         then
            Tmp := Global.Handles_List;
            while Tmp /= null loop
               Cache_Settings (Tmp);
               Tmp := Tmp.Next;
            end loop;
         end if;
      end if;
   end Set_Active;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active
      (Handle : not null access Trace_Handle_Record'Class) return Boolean is
   begin
      return not Global.Finalized and then Handle.Active;
   end Is_Active;

   ---------------
   -- Unit_Name --
   ---------------

   function Unit_Name
      (Handle : not null access Trace_Handle_Record'Class) return String is
   begin
      return Handle.Name.all;
   end Unit_Name;

   -----------------------------
   -- Create_Exception_Handle --
   -----------------------------

   procedure Create_Exception_Handle (Handle : not null Trace_Handle) is
      S       : Trace_Stream;
   begin
      if Handle.Exception_Handle = null then
         --  Unless the config file specified an explicit stream,
         --  we inherit the one from Handle.
         if Handle.Stream = Global.Streams_List then
            S := null;
         else
            S := Handle.Stream;
         end if;

         Handle.Exception_Handle := Create_Internal
           (Unit_Name => Handle.Name.all & ".EXCEPTIONS",
            From_Config_File => False,
            Stream    => S,
            Default   => (if Handle.Active then On else Off),
            Style     => Default_Exception_Style);

         Cache_Settings (Handle.Exception_Handle);
      end if;
   end Create_Exception_Handle;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Handle : not null access Trace_Handle_Record'Class;
      E      : Ada.Exceptions.Exception_Occurrence;
      Msg    : String := "Unexpected exception: ";
      Color  : String) is
   begin
      if Debug_Mode then
         if Handle.With_Colors then
            Trace (Handle, E, Color & Msg);
         else
            Trace (Handle, E, Msg);
         end if;
      end if;
   end Trace;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Handle : not null access Trace_Handle_Record'Class;
      E      : Ada.Exceptions.Exception_Occurrence;
      Msg    : String := "Unexpected exception: ";
      Style  : Message_Style := Use_Default_Style) is
   begin
      if Debug_Mode
        and then not Global.Finalized  --  module not terminated
      then
         Create_Exception_Handle (Trace_Handle (Handle));
         Trace (Handle.Exception_Handle,
                Msg & Ada.Exceptions.Exception_Information (E),
                Style => Style);
      end if;
   end Trace;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Handle   : not null access Trace_Handle_Record'Class;
      Message  : String;
      Color    : String;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if Debug_Mode then
         if Handle.With_Colors then
            Trace (Handle, Color & Message,
                   Location => Location, Entity => Entity);
         else
            Trace (Handle, Message, Location => Location, Entity => Entity);
         end if;
      end if;
   end Trace;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Handle   : not null access Trace_Handle_Record'Class;
      Message  : String;
      Style    : Message_Style := Use_Default_Style;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity)
   is
      --  We want maximum performance for traces. This saves about 2%
      --  in single-threaded applications and sometimes 1% for multi-threaded
      --  apps.
      pragma Suppress (All_Checks);

      Merged_Style : Message_Style;

   begin
      --  Do not output anything until we have called Parse_Config_File,
      --  and do not output anything after we have called Finalized and
      --  potentially freed Handle.
      --  The stream is null if the trace was Create-d as On by default
      --  but Parse_Config_File was never called.

      if not Active (Handle) or else Handle.Stream = null then
         return;
      end if;

      declare
         Start, Last  : Natural;
         Indent : constant Integer := Integer (Handle.Stream.Indentation);
         With_Color   : constant Boolean := Handle.With_Colors;
         Msg          : Msg_Strings.XString;
      begin
         for D in 1 ..  Global.Active_Last loop
            Global.Active_Decorators (D).Start_Of_Line
               (Msg, Is_Continuation => False);
         end loop;

         if Indent > 0 then
            Msg.Append ((1 .. Indent * 3 => ' '));
         end if;

         if With_Color then
            Merged_Style := Handle.Default_Style;

            if Style.Fg /= GNATCOLL.Terminal.Unchanged then
               Merged_Style.Fg := Style.Fg;
            end if;

            if Style.Bg /= GNATCOLL.Terminal.Unchanged then
               Merged_Style.Bg := Style.Bg;
            end if;

            if Style.Style /= GNATCOLL.Terminal.Unchanged then
               Merged_Style.Style := Style.Style;
            end if;

            Msg.Append (Cyan_Fg);
         end if;

         Msg.Append ('[');
         Msg.Append (Handle.Name.all);
         Msg.Append (']');
         Msg.Append (' ');

         --  Decorate before the message

         for D in 1 ..  Global.Active_Last loop
            Global.Active_Decorators (D).Before_Message
              (Trace_Handle (Handle), Msg);
         end loop;

         --  Add the message

         if Global.Split_Lines /= null and then Global.Split_Lines.Active then
            Start := Message'First;
            loop
               Last := Start;
               while Last <= Message'Last
                 and then Message (Last) /= ASCII.LF
               loop
                  Last := Last + 1;
               end loop;

               if With_Color then
                  Msg.Append
                     (GNATCOLL.Terminal.Get_ANSI_Sequence (Merged_Style));
               end if;

               Msg.Append (Message (Start .. Last - 1));

               Start := Last + 1;
               exit when Start > Message'Last;

               Msg.Append (ASCII.LF);

               for D in 1 ..  Global.Active_Last loop
                  Global.Active_Decorators (D).Start_Of_Line
                     (Msg, Is_Continuation => True);
               end loop;

               if Indent > 0 then
                  Msg.Append ((1 .. Indent * 3 => ' '));
               end if;

               if With_Color then
                  Msg.Append (Purple_Fg);
               end if;

               Msg.Append ('_');
               Msg.Append (Handle.Name.all);
               Msg.Append ('_');
               Msg.Append (' ');
            end loop;
         else
            if With_Color then
               Msg.Append
                  (GNATCOLL.Terminal.Get_ANSI_Sequence (Merged_Style));
            end if;

            Msg.Append (Message);
         end if;

         --  Decorate after the message

         if Global.Active_Last /= 0 then
            if With_Color then
               Msg.Append (Brown_Fg);
            end if;

            Msg.Append (' ');

            for D in 1 ..  Global.Active_Last loop
               Global.Active_Decorators (D).After_Message
                 (Trace_Handle (Handle), Msg);
            end loop;

            --  Remove trailing space if needed

            if Handle.With_Time then
               Put_Absolute_Time (Msg);
            end if;

            if Global.Location.Active then
               Msg.Append ("(loc: ");
               Msg.Append (Location);
               Msg.Append (')');
            end if;

            if Global.Enclosing_Entity.Active then
               Msg.Append ("(entity:");
               Msg.Append (Entity);
               Msg.Append (')');
            end if;

            Msg.Trim (Ada.Strings.Right);
         end if;

         if With_Color then
            Msg.Append (Reset_All);
         end if;

         Msg.Append (ASCII.LF);
         Handle.Stream.Put (Msg);
      end;

   exception
      when others =>
         case On_Exception is
            when Propagate =>
               raise;
            when Ignore =>
               null;
            when Deactivate =>
               begin
                  Close (Handle.Stream.all);
               exception
                  when others =>
                     null;
               end;
         end case;
   end Trace;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Handle             : not null access Trace_Handle_Record'Class;
      Condition          : Boolean;
      Error_Message      : String;
      Message_If_Success : String := "";
      Raise_Exception    : Boolean := True;
      Location           : String := GNAT.Source_Info.Source_Location;
      Entity             : String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if Active (Handle) then
         if not Condition then
            Create_Exception_Handle (Trace_Handle (Handle));
            Trace
              (Handle.Exception_Handle,
               Error_Message,
               Location => Location,
               Entity   => Entity);

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
     (Handle   : access Trace_Handle_Record'Class := null;
      Msg      : String := "";
      Style    : Message_Style := Use_Default_Style;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if Handle /= null and then Handle.Stream /= null then
         if Msg /= "" then
            Trace (Handle, Msg, Style, Location => Location, Entity => Entity);
         end if;

         if Handle.Active then
            Increment (Handle.Stream.Indentation);
         end if;
      end if;
   end Increase_Indent;

   ---------------------
   -- Decrease_Indent --
   ---------------------

   procedure Decrease_Indent
     (Handle   : access Trace_Handle_Record'Class := null;
      Msg      : String := "";
      Style    : Message_Style := Use_Default_Style;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if Handle /= null and then Handle.Stream /= null then

         --  The counter is a modulo type
         if Handle.Active then
            if Sync_Sub_And_Fetch
              (Handle.Stream.Indentation'Unchecked_Access, 1) = Minus_One
            then
               Handle.Stream.Indentation := 0;
               Trace (Handle, "Indentation error: too many decrease");
            end if;
         end if;

         if Msg /= "" then
            Trace (Handle, Msg, Style, Location => Location, Entity => Entity);
         end if;
      end if;
   end Decrease_Indent;

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

      if Ss > 0.999 then
         return 999;
      else
         return Integer (Ss * 1000.0);
      end if;
   end Local_Sub_Second;

   -----------------------
   -- Put_Absolute_Time --
   -----------------------

   procedure Put_Absolute_Time (Msg : in out Msg_Strings.XString) is
      T  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Z  : String (1 .. 3) := "000";
      Ms : constant String := Integer'Image (Local_Sub_Second (T));
   begin
      Z (3 + 1 - (Ms'Length - 1) .. 3) := Ms (Ms'First + 1 .. Ms'Last);
      if Global.Absolute_Date.Active then
         if Global.Absolute_Time.Active then
            if Global.Micro_Time.Active then
               Msg.Append ("(" & Image (T, ISO_Date & " %T:%e") & ')');
            else
               Msg.Append ("(" & Image (T, ISO_Date & " %T.") & Z & ')');
            end if;
         else
            Msg.Append ("(" & Image (T, ISO_Date) & ')');
         end if;

      else
         if Global.Micro_Time.Active then
            Msg.Append ("(" & Image (T, ISO_Date & " %T:%e") & ')');
         else
            Msg.Append ("(" & Image (T, "%T.") & Z & ')');
         end if;
      end if;
   end Put_Absolute_Time;

   --------------------
   -- Before_Message --
   --------------------

   overriding procedure Before_Message
      (Self   : in out Count_Trace;
       Handle : not null Trace_Handle;
       Msg    : in out Msg_Strings.XString)
   is
      --  ??? Should we lock to get consistent counters ?
      Total : constant Atomic_Counter :=
         Sync_Add_And_Fetch (Self.Count'Unchecked_Access, 1);
      Local : constant Atomic_Counter :=
         Sync_Add_And_Fetch (Handle.Count'Unchecked_Access, 1);
      C : constant String := Atomic_Counter'Image (Total);
      H : constant String := Atomic_Counter'Image (Local);
   begin
      Msg.Append (H (H'First + 1 .. H'Last)
         & '/' & C (C'First + 1 .. C'Last) & ' ');
   end Before_Message;

   -------------------
   -- After_Message --
   -------------------

   overriding procedure After_Message
      (Self   : in out Memory_Trace;
       Handle : not null Trace_Handle;
       Msg    : in out Msg_Strings.XString)
   is
      pragma Unreferenced (Handle);
      use GNATCOLL.Memory;
      Watermark : constant Watermark_Info := Get_Allocations;
   begin
      Msg.Append
         ("[Watermark:"
          & (if Watermark.Current > Self.Previous then '>' else '<')
          & Watermark.Current'Img & '/'
          & Watermark.High'Img & "]");
      Self.Previous := Watermark.Current;
   end After_Message;

   -------------------
   -- After_Message --
   -------------------

   overriding procedure After_Message
      (Self   : in out Ada_Memory_Trace;
       Handle : not null Trace_Handle;
       Msg    : in out Msg_Strings.XString)
   is
      pragma Unreferenced (Handle);
      use GNATCOLL.Memory;
      Watermark : constant Watermark_Info := Get_Ada_Allocations;
   begin
      if Watermark.High /= 0 then
         Msg.Append
            ("[AdaWatermark:"
             & (if Watermark.Current > Self.Previous then '>' else '<')
             & Watermark.Current'Img & '/'
             & Watermark.High'Img & "]");
      end if;
      Self.Previous := Watermark.Current;
   end After_Message;

   -------------------
   -- After_Message --
   -------------------

   overriding procedure After_Message
      (Self   : in out Elapse_Time_Trace;
       Handle : not null Trace_Handle;
       Msg    : in out Msg_Strings.XString)
   is
      pragma Unreferenced (Self);
      T   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Dur : Integer;
   begin
      if Handle.Timer /= No_Time then
         Dur := Integer ((T - Handle.Timer) * 1000);
         Msg.Append ("(elapsed:" & Integer'Image (Dur) & "ms)");
      end if;
      Handle.Timer := T;
   end After_Message;

   -------------------
   -- After_Message --
   -------------------

   overriding procedure After_Message
      (Self   : in out Stack_Trace;
       Handle : not null Trace_Handle;
       Msg    : in out Msg_Strings.XString)
   is
      pragma Unreferenced (Self, Handle);
      Tracebacks : GNAT.Traceback.Tracebacks_Array (1 .. 50);
      Len        : Natural;
   begin
      Call_Chain (Tracebacks, Len);
      Msg.Append ("(callstack: ");
      for J in Tracebacks'First .. Len loop
         Msg.Append (System.Address_Image (Get_PC (Tracebacks (J))) & ' ');
      end loop;
      Msg.Append (")");
   end After_Message;

   --------------------------
   -- Add_Global_Decorator --
   --------------------------

   procedure Add_Global_Decorator
      (Decorator : not null Trace_Decorator;
       Name      : String) is
   begin
      Register_Handle (Trace_Handle (Decorator), To_Upper (Name));
      Decorator.Active := False;

      --  Set this flag, so that a "+" in the config file has no impact on
      --  decorators.
      Decorator.Forced_Active := True;
   end Add_Global_Decorator;

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
      Lock (Global.Lock);
      Global.Factories_List := new Stream_Factories'
        (Name    => new String'("&" & Name),
         Factory => Factory,
         Next    => Global.Factories_List);
      Unlock (Global.Lock);
   end Register_Stream_Factory;

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

   overriding procedure Put
      (Stream     : in out File_Stream_Record;
       Str        : Msg_Strings.XString)
   is
      N       : size_t;
      S       : Msg_Strings.Char_Array;
      L       : Natural;
   begin
      --  fwrite is thread safe on Windows and POSIX systems,
      --  we should not need locking.

      Str.Get_String (S, L);

      --  The call to fwrite is C, so will not raise exceptions
      Lock (Stream.Lock);
      N := fwrite
         (buffer  => S.all'Address,
          size    => size_t (L),
          count   => 1,
          stream  => Stream.File);
      Unlock (Stream.Lock);

      if N /= size_t (L) then
         --   ??? Could not write to file, disk full ?
         null;
      end if;
   end Put;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Stream : in out File_Stream_Record) is
      Status : int;
      pragma Unreferenced (Status);
   begin
      if Stream.File /= stdout
         and then Stream.File /= stderr
      then
         Status := fclose (Stream.File);
         Stream.File := NULL_Stream;
      end if;

      GNAT.Strings.Free (Stream.File_Name);

      Close (Trace_Stream_Record (Stream));
   end Close;

   ------------------
   -- Parse_Config --
   ------------------

   procedure Parse_Config
     (Config           : String;
      On_Exception     : On_Exception_Mode := Propagate;
      Force_Activation : Boolean := True;
      Relative_Path_To : GNATCOLL.VFS.Virtual_File :=
         GNATCOLL.VFS.Get_Current_Dir)
   is
      Handle : Trace_Handle;
      Dec    : Trace_Decorator;
      Count  : Natural := 0;

      procedure Create_Decorators;
      --  Create all default decorators, if not done yet

      function One_Line (Line : String) return Boolean;
      --  Callback function for each line of the configuration

      -----------------------
      -- Create_Decorators --
      -----------------------

      procedure Create_Decorators is
      begin
         if Global.Colors = null then
            Set_Default_Stream ("&1");

            Global.Micro_Time := new Trace_Decorator_Record;
            Add_Global_Decorator (Global.Micro_Time, "DEBUG.MICRO_TIME");

            Dec := new Elapse_Time_Trace;
            Add_Global_Decorator (Dec, "DEBUG.ELAPSED_TIME");

            Dec := new Stack_Trace;
            Add_Global_Decorator (Dec, "DEBUG.STACK_TRACE");

            Dec := new Count_Trace;
            Add_Global_Decorator (Dec, "DEBUG.COUNT");

            Dec := new Memory_Trace;
            Add_Global_Decorator (Dec, "DEBUG.MEMORY");

            Dec := new Ada_Memory_Trace;
            Add_Global_Decorator (Dec, "DEBUG.ADA_MEMORY");

            --  These are handled directly in Trace, but we should have them on
            --  the active list of decorators to know whether we need to add a
            --  space.

            Global.Absolute_Time := new Trace_Decorator_Record;
            Add_Global_Decorator (Global.Absolute_Time, "DEBUG.ABSOLUTE_TIME");

            Global.Absolute_Date := new Trace_Decorator_Record;
            Add_Global_Decorator (Global.Absolute_Date, "DEBUG.ABSOLUTE_DATE");

            Global.Enclosing_Entity := new Trace_Decorator_Record;
            Add_Global_Decorator
               (Global.Enclosing_Entity, "DEBUG.ENCLOSING_ENTITY");

            Global.Location := new Trace_Decorator_Record;
            Add_Global_Decorator (Global.Location, "DEBUG.LOCATION");

            --  The following are not decorators, and handled specially

            Global.Finalize_Traces := new Trace_Decorator_Record;
            Add_Global_Decorator
               (Global.Finalize_Traces, "DEBUG.FINALIZE_TRACES");
            Global.Finalize_Traces.Active := True;

            Global.Split_Lines := new Trace_Decorator_Record;
            Add_Global_Decorator (Global.Split_Lines, "DEBUG.SPLIT_LINES");
            Global.Split_Lines.Active := True;

            Global.Colors := new Trace_Decorator_Record;
            Add_Global_Decorator (Global.Colors, "DEBUG.COLORS");
         end if;
      end Create_Decorators;

      S : constant String := "[ \t]*";

      Line_Re : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile
         ("^(?:"

          & "([^\s=:>+-]+)" & S           --  1 = name
          & "(?:=" & S & "(yes|no))?" & S --  2 = active?
          & "(:[^\s>]+)?" & S             --  3 = options
          & "(?:>>?" & S & "(\S+))?" & S    --  4 = stream

          & "|"
          & "(>>?\S+)?" & S                 --  5 = default stream

          & "|"
          & "(\+)" & S                    --  6 = "+"

          & ")?"                          --  line can be empty
          & S & "(?:(?:#|--).*)?"         --  end of line comments
          & "\r?$");

      function One_Line (Line : String) return Boolean is
         M : GNAT.Regpat.Match_Array (0 .. 6);
         Group_Name            : constant := 1;
         Group_Active          : constant := 2;
         Group_Options         : constant := 3;
         Group_Stream          : constant := 4;
         Group_Default_Stream  : constant := 5;
         Group_All             : constant := 6;
      begin
         Count := Count + 1;

         if Line = "" then
            return True;
         end if;

         Match (Line_Re, Line, Matches => M);

         if M (0) = No_Match then
            if On_Exception = Propagate then
               raise Constraint_Error with
                 "Line " & Count'Img & ": """ & Line & """ is not recognised.";
            end if;

         elsif M (Group_All) /= No_Match then
            Global.Default_Activation := True;
            Handle := Global.Handles_List;
            while Handle /= null loop
               if not Handle.Forced_Active then
                  Set_Active (Handle, True);

                  --  A later declaration of the stream in the code
                  --  should not be allowed to reset Active to False
                  Handle.Forced_Active := True;
               end if;
               Handle := Handle.Next;
            end loop;

         elsif M (Group_Default_Stream) /= No_Match then
            Set_Default_Stream
              (Config (M (Group_Default_Stream).First ..
                 M (Group_Default_Stream).Last),
               Config_File => Relative_Path_To);

         elsif M (Group_Name) /= No_Match then
            declare
               Active : constant Default_Activation_Status :=
                 (if M (Group_Active) = No_Match
                  or else Config (M (Group_Active).First ..
                      M (Group_Active).Last) /= "no"
                  then On
                  else Off);
               Stream : Trace_Stream := null;
               Style  : Message_Style := Default_Style;
            begin
               --  Do we have options for this handle ?

               if M (Group_Options) /= No_Match then
                  declare
                     use GNATCOLL.Terminal;
                     Options : String_List_Access := Split
                       (Config (M (Group_Options).First ..
                          M (Group_Options).Last),
                       ':');
                  begin
                     for Opt of Options.all loop
                        if Starts_With (Opt.all, "fg=") then
                           Style.Fg := ANSI_Color'Value
                             (Opt (Opt'First + 3 .. Opt'Last));
                        elsif Starts_With (Opt.all, "bg=") then
                           Style.Bg := ANSI_Color'Value
                             (Opt (Opt'First + 3 .. Opt'Last));
                        elsif Starts_With (Opt.all, "style=") then
                           Style.Style := ANSI_Style'Value
                             (Opt (Opt'First + 6 .. Opt'Last));
                        end if;
                     end loop;

                     Free (Options);
                  end;
               end if;

               --  What stream is this sent to ?

               if M (Group_Stream) /= No_Match then
                  declare
                     Save   : Integer := M (Group_Stream).First;
                     Append : Boolean := False;
                  begin
                     if Save + 1 <= M (Group_Stream).Last
                       and then Config (Save) = '>'
                     then
                        Append := True;
                        Save := Save + 1;
                     end if;

                     Stream := Find_Stream
                       (Config (Save .. M (Group_Stream).Last),
                        Relative_Path_To, Append);
                  end;
               end if;

               Handle := Create_Internal
                 (Config (M (Group_Name).First .. M (Group_Name).Last),
                  From_Config_File => True,
                  Default          => Active,
                  Style            => Style,
                  Stream           => Stream);
            end;
         end if;

         return True;
      end One_Line;

   begin
      if not Debug_Mode then
         return;
      end if;

      GNATCOLL.Traces.On_Exception := On_Exception;

      if Force_Activation or else Config /= "" then
         Create_Decorators;
      end if;

      if Config /= "" then
         Split (Config, (1 => ASCII.LF), One_Line'Access);
      end if;
   end Parse_Config;

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
      Buffer    : Str_Access;
      File      : Mapped_File;
   begin
      if not Debug_Mode then
         return;
      end if;

      GNATCOLL.Traces.On_Exception := On_Exception;

      if File_Name = No_File then
         Parse_Config
            ("", On_Exception, Force_Activation => Force_Activation);
      else
         begin
            File := Open_Read (+File_Name.Full_Name);
         exception
            when Ada.IO_Exceptions.Name_Error =>
               Parse_Config
                  ("", On_Exception, Force_Activation => Force_Activation);
               return;
         end;

         Read (File);
         Buffer := Data (File);

         Parse_Config
            (Config           => String (Buffer (1 .. Last (File))),
             On_Exception     => On_Exception,
             Force_Activation => Force_Activation,
             Relative_Path_To => File_Name.Dir);
         Close (File);
      end if;
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
      if not Global.Finalized
         --  Might never have been initialized at all
         and then Global.Finalize_Traces /= null
         and then Global.Finalize_Traces.Active
      then
         Lock (Global.Lock);
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

         Unlock (Global.Lock);
      end if;

      Global.Finalized := True;
   end Finalize;

   ---------------------
   -- For_Each_Handle --
   ---------------------

   procedure For_Each_Handle (Proc : not null Handlers_Proc) is
      Tmp : Trace_Handle := Global.Handles_List;
   begin
      while Tmp /= null loop
         Proc (Tmp);
         Tmp := Tmp.Next;
      end loop;
   end For_Each_Handle;

   ------------------------
   -- Set_Default_Stream --
   ------------------------

   procedure Set_Default_Stream
      (Name        : String;
       Config_File : Virtual_File := No_File)
   is
      S : Trace_Stream;
      T : Trace_Stream;
      H : Trace_Handle;

   begin
      if Name'Length > 2
        and then Name (Name'First .. Name'First + 1) = ">>"
      then
         S := Find_Stream
           (Name (Name'First + 2 .. Name'Last),
            Config_File.Dir, Append => True);
      elsif Name (Name'First) = '>' then
         S := Find_Stream
            (Name (Name'First + 1 .. Name'Last),
             Config_File.Dir, Append => False);
      else
         S := Find_Stream (Name, Config_File.Dir, Append => False);
      end if;

      if S /= null then
         --  Put it first in the list

         Lock (Global.Lock);
         if Global.Streams_List /= S then
            T := Global.Streams_List;
            while T.Next /= S loop
               T := T.Next;
            end loop;

            T.Next := S.Next;
            S.Next := Global.Streams_List;
            Global.Streams_List := S;
         end if;

         --  Apply the default stream for all streams that do not have an
         --  explicit one

         H := Global.Handles_List;
         while H /= null loop
            if H.Stream = null or else H.Stream_Is_Default then
               H.Stream := S;
               H.Stream_Is_Default := True;
               Cache_Settings (H);
            end if;
            H := H.Next;
         end loop;

         Unlock (Global.Lock);
      end if;
   end Set_Default_Stream;

   -----------
   -- Count --
   -----------

   function Count
      (Handler : not null access Trace_Handle_Record'Class) return Natural is
   begin
      return Natural (Handler.Count);
   end Count;

   ------------
   -- Create --
   ------------

   function Create
      (Handle   : Trace_Handle;
       Message  : String := "";
       Location : String := GNAT.Source_Info.Source_Location;
       Entity   : String := GNAT.Source_Info.Enclosing_Entity;
       Style    : Message_Style := Default_Block_Style)
      return Block_Trace_Handle is
   begin
      return Result : Block_Trace_Handle do
         if Active (Handle) then
            Result.Me := Handle;
            Result.Style := Style;
            Result.Loc := new String'(Entity & ':' & Location);
            if Message /= "" then
               Increase_Indent
                  (Handle, "Entering " & Result.Loc.all & ' ' & Message,
                   Style => Style, Location => "", Entity => "");
            else
               Increase_Indent
                  (Handle, "Entering " & Result.Loc.all,
                   Style => Style, Location => "", Entity => "");
            end if;
         end if;
      end return;
   end Create;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Block_Trace_Handle) is
   begin
      --  If we were active when Create was called
      if Self.Me /= null then
         Decrease_Indent
            (Self.Me, "Leaving " & Self.Loc.all,
             Style    => Self.Style,
             Location => "",   --  avoid duplicate info in the output
             Entity   => "");
      end if;
      Free (Self.Loc);
   end Finalize;

end GNATCOLL.Traces;
