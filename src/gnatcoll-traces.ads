------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

--  Logging framework

with GNAT.Source_Info;
with GNAT.Strings;
with Ada.Calendar;
with Ada.Exceptions;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package GNATCOLL.Traces is

   type Trace_Handle_Record is tagged private;
   type Trace_Handle is access all Trace_Handle_Record'Class;
   --  A handle for a trace stream.
   --  One such handle should be created for each module/unit/package where it
   --  is relevant. They are associated with a specific name and output stream,
   --  and can be activated through a configuration file. If two or more
   --  packages create streams with the same name, they will share their
   --  attributes.

   Config_File_Environment : constant String := "ADA_DEBUG_FILE";
   Default_Config_File     : constant Filesystem_String := ".gnatdebug";
   --  Name of the default configuration file. This file is looked for first in
   --  the current directory, then in the user's home directory.  If no file is
   --  found, then no handle will be activated.  The name of this file can be
   --  overriden by the environment variable Config_File_Environment, which
   --  should be an absolute name (or relative to the current directory). If
   --  this variable is set, the standard file is never searched.
   --
   --  The format of the configuration file is the following:
   --    * activating a module:
   --      MODULE_NAME=yes
   --      MODULE_NAME
   --    * deactivating a module (default)
   --      MODULE_NAME=no
   --    * redirecting all modules to a different stream:
   --      - to a file:
   --        >filename
   --        If filename is a relative path, it is relative to the location of
   --        the configuration file. $$ is automatically replaced by the
   --        process number. $D is automatically replaced by the current date.
   --        $T is automatically replaced by the current date and time.
   --        You can use >>filename instead if you want to append to the file.
   --      - to standard output
   --        >&1
   --      - to standard error
   --        >&2
   --      - to a user-defined stream (see gnat-traces-syslog.ads):
   --        >&stream
   --    * redirecting a specific module to a file
   --      MODULE_NAME=yes >filename
   --      MODULE_NAME=yes >&stream
   --    * comments
   --      -- comment
   --    * Activate traces for all modules, unless explicitely deactivated in
   --      the lines following the '+'
   --      +
   --      Note that this doesn't apply to the predefined entities (see below)
   --
   --  It is also possible to substitute a module name with a '*', to configure
   --  a whole set of modules with a single line. For instance:
   --
   --    *  *.EXCEPTIONS=yes >&stdout
   --       will always display a stream whose name ends with ".EXCEPTIONS" to
   --       stdout.
   --
   --    *  MODULE_NAME.*=no
   --       Disables all streams starting with "MODULE_NAME" (including
   --       MODULE_NAME itself). The star can only be used to substitute the
   --       whole first or last name. If the configuration file also contains
   --       a line like "MODULE_NAME.FOO" anywhere (before or after), then this
   --       specific stream is not disabled.
   --
   --  Here is a short example of configuration file:
   --     +                 --  by default, show all
   --     >&2               --  defines the default stream
   --     PKG1=no           --  do not show
   --     PKG2=yes          --  to the default stream, ie stderror
   --     PKG3=yes >file    --  to the file "file" in current directory
   --     PKG4=yes >&syslog --  to syslog, see gnat-traces-syslog.ads

   Debug_Mode : constant Boolean := True;
   --  Set the global activation status for the debug traces. If this is set to
   --  False and the subprograms below are inlined, then no code will be
   --  generated to support debug traces. Otherwise, if Debug_Mode is True,
   --  then the debug traces can be activated selectively for each module.

   type On_Exception_Mode is (Propagate, Ignore, Deactivate);
   --  Behavor when an exception is raised while writing to the log stream e.g
   --  because of NFS error when writing to a file.
   --    Propagate:  the exception is propagated
   --    Ignore:     the exception is silently ignored
   --    Deactivate: when an exception is raised when manipulating a handle
   --                deactivate it; no logging will happen on this handle
   --                anymore.

   procedure Parse_Config_File
     (Filename         : Virtual_File;
      Default          : Virtual_File := No_File;
      On_Exception     : On_Exception_Mode := Propagate;
      Force_Activation : Boolean := True);
   --  Initializes this package, and parse the configuration file. The
   --  algorithm is the following:
   --    - If filename is specified and exists on the disk, parse this file
   --    - Else test the file described in Config_File_Environment
   --    - If not found, search in the current directory for a file
   --      Default_Config_File
   --    - If not found, search in the user's home directory for a file
   --      Default_Config_File
   --    - If still not found, parses Default
   --  On_Exception is used to define the behavior should something unexpected
   --  prevent the log stream to be written.
   --
   --  Until at least one file is parsed, this package will never output
   --  anything, unless Force_Activation is set, in which case the default
   --  trace_handle status will apply after the call to Parse_Config_File even
   --  if Filename or Default are unspecified.

   procedure Parse_Config_File
     (Filename         : String := "";
      Default          : String := "";
      On_Exception     : On_Exception_Mode := Propagate;
      Force_Activation : Boolean := True);
   --  Same as above, using regular strings for file names.

   type Output_Proc is access procedure (Str : String);
   procedure Show_Configuration (Output : Output_Proc);
   --  Output on Output the current configuration for all traces. The resulting
   --  file is a valid configuration file, which can be reused in later runs.

   procedure Finalize;
   --  Free all the registered handles. This is not strictly needed, but is
   --  specially useful when testing memory leaks in your application. This
   --  also ensures that output streams are correctly closed.

   type Handle_Factory is access function return Trace_Handle;

   type Default_Activation_Status is (From_Config, On, Off);
   function Create
     (Unit_Name : String;
      Default   : Default_Activation_Status := From_Config;
      Stream    : String := "";
      Factory   : Handle_Factory := null;
      Finalize  : Boolean := True)
      return Trace_Handle;
   --  Create a new handle
   --  Unit_Name is upper-cased, and looked-for in the configuration file to
   --  check whether traces should be emitted for that module. Calling this
   --  function several times with the same Unit_Name will always return the
   --  same handle.
   --
   --  If Default is not From_Config, this forces an explicit activation
   --  status for that handle. To change it, the user must explicitely have
   --  a line for this handle in the config file, and this handle is not
   --  impacted by the use of "+" in this config file.
   --
   --  Stream indicates which stream the application is sent to. This has the
   --  same format as in the configuration file:
   --     - if left to the empty string, this is the default stream specified
   --       in the configuration file on the line starting with ">".
   --     - otherwise, the string is similar to what can be specified in the
   --       configuration file after ">", ie one of "filename", "&2" (stderr),
   --       "&1" (stdout) or any of the registered streams ("&syslog" for
   --       instance, see gnat-traces-syslog.ads)
   --  If no such stream is found, defaults on the default stream declared in
   --  the config file.
   --
   --  If the handle has not been created yet in some other part of the code,
   --  a new one will be allocated. Factory can be used in this case to do the
   --  actual allocation, so that you can return your own Trace_Handle_Record,
   --  which might override some of primitive operations like the decorators.
   --  If Factory is unspecified, a standard Trace_Handle_Record is allocated.
   --
   --  If Finalize is True, the handle will be freed when Finalize is called,
   --  otherwise it won't be. The only reason to set this to False is so that
   --  the handle still exists when the application itself is being finalized
   --  by the compiler, so that you can have logs till the last minute.

   function Unit_Name (Handle : Trace_Handle) return String;
   --  Return the unit name (upper-cased) for this handle. This can be used for
   --  instance in generic packages to specialize the handle for a specific
   --  instance.
   --
   --  Recommended use with generics:
   --  Since generics might be used in various, independent modules, the
   --  recommended use is to have one more generic parameter for the trace
   --  handle. Internally, it is then possible to specialize this stream (see
   --  the subprogram Unit_Name):
   --     generic
   --         Self_Debug : Trace_Handle := Create ("My_Generic");
   --     package My_Generic is
   --         Me : Trace_Handle := Create ("Generic" & Unit_Name (Self_Debug));
   --         ...

   Red_Fg     : constant String := ASCII.ESC & "[31m";
   Green_Fg   : constant String := ASCII.ESC & "[32m";
   Brown_Fg   : constant String := ASCII.ESC & "[33m";
   Blue_Fg    : constant String := ASCII.ESC & "[34m";
   Purple_Fg  : constant String := ASCII.ESC & "[35m";
   Cyan_Fg    : constant String := ASCII.ESC & "[36m";
   Grey_Fg    : constant String := ASCII.ESC & "[37m";
   Default_Fg : constant String := ASCII.ESC & "[39m";

   Red_Bg     : constant String := ASCII.ESC & "[41m";
   Green_Bg   : constant String := ASCII.ESC & "[42m";
   Brown_Bg   : constant String := ASCII.ESC & "[43m";
   Blue_Bg    : constant String := ASCII.ESC & "[44m";
   Purple_Bg  : constant String := ASCII.ESC & "[45m";
   Cyan_Bg    : constant String := ASCII.ESC & "[46m";
   Grey_Bg    : constant String := ASCII.ESC & "[47m";
   Default_Bg : constant String := ASCII.ESC & "[49m";
   --  The various colors that can be applied to text. You can combine a
   --  foreground and a background color by concatenating the strings.

   procedure Trace
     (Handle : Trace_Handle;
      E      : Ada.Exceptions.Exception_Occurrence;
      Msg    : String := "Unexpected exception: ";
      Color  : String := Default_Fg);
   pragma Inline (Trace);
   --  Extract information from the given Exception_Occurence and output it
   --  with Msg as a prefix.
   --  You can override the default color used for the stream by specifying the
   --  color parameter.
   --
   --  The output is really done on a separate handle with the same name as
   --  Handle and a suffix of ".EXCEPTIONS". This way, it is possible to
   --  configure this handle differently. For instance, the configuration file
   --  could contain:
   --     *.EXCEPTIONS=yes >&stdout
   --  to always display those exceptions on stdout and not on the default
   --  stream for Handle itself (a useful scenario when this version of Trace
   --  is used to display unexpected exceptions in your application).

   procedure Trace
     (Handle   : Trace_Handle;
      Message  : String;
      Color    : String := Default_Fg;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity);
   pragma Inline (Trace);
   --  Output Message to the stream associated with Handle, along with any
   --  extra information setup by the user (see the default handles below).
   --  If Handle is not active, this function will do nothing.
   --
   --  If message includes ASCII.LF characters, then several lines are output,
   --  starting with a special prefix
   --
   --  Do not modify the parameters Location and Entity, they will have proper
   --  default values, and are used to output additional information about the
   --  context of the call.
   --
   --  In case of exception (for instance because the log file is not
   --  writable), the behavior is controlled by the parameter On_Exception
   --  that was passed to Parse_Config_File.

   procedure Assert
     (Handle             : Trace_Handle;
      Condition          : Boolean;
      Error_Message      : String;
      Message_If_Success : String := "";
      Raise_Exception    : Boolean := True;
      Location           : String := GNAT.Source_Info.Source_Location;
      Entity             : String := GNAT.Source_Info.Enclosing_Entity);
   pragma Inline (Assert);
   --  If Condition is False, then output Error_Message to Handle.
   --  Assertion_Error is raised if Condition is False and Raise_Exception is
   --  True.
   --
   --  Condition is not tested if Handle is not active.
   --  Message_If_Success is logged if Condition is True and the message
   --  is not the empty string.

   procedure Set_Active (Handle : Trace_Handle; Active : Boolean);
   pragma Inline (Set_Active);
   --  Override the activation status for Handle.
   --  When not Active, the Trace function will do nothing.

   function Active (Handle : Trace_Handle) return Boolean;
   pragma Inline (Active);
   --  Return True if traces for Handle are actived.
   --  This function can be used to avoid the evaluation of complex
   --  expressions in case traces are not actived, as in the following
   --  code:
   --     if Active (Handle) then
   --        begin
   --           Trace (Handle, Message & Expensive_Computation);
   --        end;
   --     end if;
   --  The extra begin...end block can be used to limit the impact on the
   --  heap for the evaluation of Expensive_Computation.

   procedure Increase_Indent
     (Handle : Trace_Handle := null; Msg : String := "");
   procedure Decrease_Indent
     (Handle : Trace_Handle := null; Msg : String := "");
   --  Change the indentation level for traces. This is so that traces that
   --  result from other subprograms be slightly indented, so as to make the
   --  output more readable. The output would for instance look like:
   --       [HANDLE1] Procedure 1
   --          [HANDLE2] Procedure 2
   --       [HANDLE1] End of Procedure 1
   --  If Handle and Msg are specified, a message is output on that handle to
   --  explain the change of indentation. The message is only displayed if the
   --  handle is active, but the indentation is always changed.

   function Count (Handler : Trace_Handle) return Natural;
   --  Return the number of times that Trace was called on the handler. This
   --  count is incremented even when Handler is inactive.

   -------------
   -- Streams --
   -------------

   type Trace_Stream_Record is abstract tagged private;
   type Trace_Stream is access all Trace_Stream_Record'Class;
   --  A stream is an object responsible for ultimately displaying a string (as
   --  opposed to using Put_Line). Such objects do not need, in general, to be
   --  manipulated by your application, and you only access them by name (see
   --  the various descriptions above, and the parameter Stream in the call to
   --  Create).
   --  The various streams support various capabilities, and for instance not
   --  all of them can display colors.
   --  You could create your own stream if you want to redirect the traces to
   --  some specific area in your graphical application for instance, or
   --  because you want to output the logs on a socket and have them read by
   --  another application.
   --  A few predefined streams are provided in this package, and others in
   --  child packages (gnat-traces-syslog.ads for instance).

   procedure Put
     (Stream : in out Trace_Stream_Record; Str : String) is abstract;
   --  Outputs Str to the stream. This must not be followed by a newline
   --  character automatically. Streams that can only output a whole line at
   --  a time should buffer Str, and only print it on the next call to
   --  Newline. This module does not output a single line at once, since that
   --  would require using temporary strings (and hence memory allocation) for
   --  every trace, which is inefficient in most cases.

   procedure Newline (Stream : in out Trace_Stream_Record) is abstract;
   --  Terminates the current line of output

   procedure Close (Stream : in out Trace_Stream_Record);
   --  Close the stream

   function Supports_Color (Stream : Trace_Stream_Record) return Boolean;
   function Supports_Time  (Stream : Trace_Stream_Record) return Boolean;
   --  Whether the stream accepts color output, and whether we should output
   --  the time (if the user requested it). In some cases (syslog for instance)
   --  it isn't necessary to output the time, since that's already done
   --  automatically

   type Stream_Factory is abstract tagged null record;

   type Stream_Factory_Access is access all Stream_Factory'Class;

   function New_Stream
     (Factory : Stream_Factory; Args : String) return Trace_Stream is abstract;
   --  Return a newly allocated stream.
   --  Args is part of the string provided by the user in the configuration
   --  file (see below Register_Stream_Factory).
   --  The factory is never called twice with the same arguments, since this
   --  package will reuse existing streams whenever possible.

   procedure Register_Stream_Factory
     (Name : String; Factory : Stream_Factory_Access);
   --  Add Factory as one of the supported streams, available to Create or in
   --  the configuration files. This must be called before parsing the
   --  configuration file, of course.
   --  The following predefined streams are always registered:
   --     "&1":       output to stdout (syntax similar to Unix)
   --     "&2":       output to stderr
   --     "filename": output to a file named "filename"
   --  To avoid confusion with filenames, streams registered through this
   --  procedure will be available as:
   --     "&" & Name [ & ":" & Args ]
   --  The arguments are optional and can be used to further customize your
   --  stream.
   --  In the configuration file, you can redirect to any of the registered
   --  stream, either by default by putting the following on a line of its own:
   --      >&stream_name
   --      >&stream_name:args
   --  or for each specific stream:
   --     STREAM=yes >&stream_name
   --     STREAM=yes >&stream_name:args
   --  The object pointed by Factory will be freed by automatically when the
   --  factory container is freed.

   procedure Set_Default_Stream (Name : String);
   --  Set Name as the default string.
   --  See Register_Stream_Factory for a list of valid names. The name can be
   --  prefixed with ">>" to append to that stream.

   ----------------
   -- Decorators --
   ----------------
   --  The following decorators are predefined.
   --  They are used to output additional information with each log message,
   --  and can be activated through the configuration file as usual.
   --
   --  "DEBUG.ABSOLUTE_TIME"
   --  If this handle is activated, then the absolute time will be added to the
   --  output, if the stream supports it (syslog does not)

   --  "DEBUG.MICRO_TIME"
   --  If this handle is activated, the absolute time will be displayed using
   --  micro-seconds resolution, instead of just seconds.

   --  "DEBUG.ABSOLUTE_DATE"
   --  If this handle is activated, then the absolute date will be added to the
   --  output, if the stream supports it (syslog does not)

   --  "DEBUG.ELAPSED_TIME"
   --  If this handle is activated, then the elapsed time since the last
   --  call to Trace for this handler will be displayed.

   --  "DEBUG.STACK_TRACE"
   --  If this handle is activated, then the stack trace will be displayed.

   --  "DEBUG.LOCATION"
   --  If this is activated, then the location of the call to Trace is
   --  displayed. Note that, contrary to DEBUG.STACK_TRACE, this works on
   --  all targets, and even if the executable wasn't compiled with debug
   --  information.

   --  "DEBUG.COLORS"
   --  If this handle is activated, then the messages will use colors to
   --  separate the actual message from the information output in the
   --  stream, if the latter supports color output

   --  "DEBUG.ENCLOSING_ENTITY"
   --  If this handle is activated, the name of the enclosing entity at the
   --  location of the call to Trace will be displayed.

   --  "DEBUG.COUNT"
   --  If this handle is actived, two numbers are associated with each output
   --  trace: one of them is unique for the handle, the other is unique in the
   --  whole application life. These can for instance be used to set
   --  conditional breakpoints for a specific trace (break on traces.Log or
   --  traces.Trace, and check the value of Handle.Count

   --  "DEBUG.FINALIZE_TRACES"
   --  This handle is activated by default. If deactivated, the trace handles
   --  will never be freed when the program is finalized by the compiler. This
   --  is mostly for debugging purposes only.

   procedure Pre_Decorator
     (Handle  : in out Trace_Handle_Record;
      Stream  : in out Trace_Stream_Record'Class;
      Message : String);
   procedure Post_Decorator
     (Handle   : in out Trace_Handle_Record;
      Stream   : in out Trace_Stream_Record'Class;
      Location : String;
      Entity   : String;
      Message  : String);
   --  You can override either of these two procedures to add your own
   --  decorators (ie additional information) each time some message is logged.
   --  It is recommended that you call the inherited procedure to get access to
   --  the standard decorators.

private
   type Trace_Stream_Record is abstract tagged record
      Name          : GNAT.Strings.String_Access;
      Next          : Trace_Stream;
   end record;
   --  Name is the full name including the arguments, for instance "file:foo"
   --  if the user has defined a stream called "file" with a parameter "foo"

   type Trace_Handle_Record is tagged record
      Name          : GNAT.Strings.String_Access;
      Timer         : Ada.Calendar.Time;
      Next          : Trace_Handle;
      Stream        : Trace_Stream;  --  null for default stream

      Exception_Handle : Trace_Handle;
      --  The handle used when calling Trace and passing an exception
      --  occurrence. This has  Name & ".EXCEPTIONS" as a name, and is created
      --  the first time it is needed.

      Count         : Natural;
      Finalize      : Boolean;
      Active        : Boolean;
      Forced_Active : Boolean := False;
   end record;
   --  If Forced_Active is true, then the Active status shouldn't be impacted
   --  by a '+' in the configuration file

end GNATCOLL.Traces;
