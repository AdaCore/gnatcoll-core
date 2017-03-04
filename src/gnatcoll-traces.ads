------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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
private with Ada.Finalization;

with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GNATCOLL.Atomic;        use GNATCOLL.Atomic;
with GNATCOLL.Strings_Impl;

package GNATCOLL.Traces is

   Config_File_Environment : constant String := "ADA_DEBUG_FILE";
   Default_Config_File     : constant Filesystem_String := ".gnatdebug";
   --  Name of the default configuration file. This file is looked for first in
   --  the current directory, then in the user's home directory.  If no file is
   --  found, then no handle will be activated.  The name of this file can be
   --  overridden by the environment variable Config_File_Environment, which
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
   --      - to a file, with specific options:
   --        >filename:buffer_size=0
   --        The options are separated from the filename with a ':', and can
   --        be any of:
   --            * "buffer_size": the size of the buffer. The logs are
   --              synchronized with the disk when this buffer is full.
   --              Setting this to 0 means that synchronization appears after
   --              every output line, which is slow but might help when
   --              debugging a crashing application.
   --      - to standard output
   --        >&1
   --        >&1:buffer_size=0      (to force flushing after each line)
   --      - to standard error
   --        >&2
   --        >&2:buffer_size=0      (to force flushing after each line)
   --      - to a user-defined stream (see gnat-traces-syslog.ads):
   --        >&stream
   --        >&stream:option1:option2
   --    * redirecting a specific module to a file
   --      MODULE_NAME=yes >filename
   --      MODULE_NAME=yes >&stream
   --    * comments
   --      -- comment
   --    * Activate traces for all modules, unless explicitely deactivated in
   --      the lines following the '+'
   --      +
   --      Note that this doesn't apply to the decorators (see below)
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
   --  anything, unless Force_Activation is set.

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

   -------------
   -- Loggers --
   -------------

   type Trace_Handle_Record is tagged limited private;
   type Trace_Handle is access all Trace_Handle_Record'Class;
   subtype Logger is Trace_Handle;   --  alternative name
   --  A handle for a trace stream.
   --  One such handle should be created for each module/unit/package where it
   --  is relevant. They are associated with a specific name and output stream,
   --  and can be activated through a configuration file. If two or more
   --  packages create streams with the same name, they will share their
   --  attributes.

   type Handle_Factory is access function return Logger;

   type Default_Activation_Status is (From_Config, On, Off);
   function Create
     (Unit_Name : String;
      Default   : Default_Activation_Status := From_Config;
      Stream    : String := "";
      Factory   : Handle_Factory := null;
      Finalize  : Boolean := True)
      return Logger;
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
   --  when you need to override Trace. The factory is only called once.
   --
   --  If Finalize is True, the handle will be freed when Finalize is called,
   --  otherwise it won't be. The only reason to set this to False is so that
   --  the handle still exists when the application itself is being finalized
   --  by the compiler, so that you can have logs till the last minute.
   --  See also the "DEBUG.FINALIZE_TRACES" configuration.

   function Unit_Name
      (Handle : not null access Trace_Handle_Record'Class) return String;
   --  Return the unit name (upper-cased) for this handle. This can be used for
   --  instance in generic packages to specialize the handle for a specific
   --  instance.
   --
   --  Recommended use with generics:
   --  Since generics might be used in various, independent modules, the
   --  recommended use is to have one more generic parameter for the logger
   --  Internally, it is then possible to specialize this stream (see
   --  the subprogram Unit_Name):
   --     generic
   --         Self_Debug : Logger := Create ("My_Generic");
   --     package My_Generic is
   --         Me : Logger := Create ("Generic" & Unit_Name (Self_Debug));
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
     (Handle : not null access Trace_Handle_Record'Class;
      E      : Ada.Exceptions.Exception_Occurrence;
      Msg    : String := "Unexpected exception: ";
      Color  : String := Default_Fg);
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
     (Handle   : not null access Trace_Handle_Record'Class;
      Message  : String;
      Color    : String := Default_Fg;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity);
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
   --
   --  You can override this procedure if you systematically want to add extra
   --  information when logging via a specific handle. The other procedures
   --  like Assert, Increase_Indent and Decrease_Indent will call this
   --  procedure.

   procedure Assert
     (Handle             : not null access Trace_Handle_Record'Class;
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

   procedure Increase_Indent
     (Handle   : access Trace_Handle_Record'Class := null;
      Msg      : String := "";
      Color    : String := Default_Fg;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity);
   procedure Decrease_Indent
     (Handle   : access Trace_Handle_Record'Class := null;
      Msg      : String := "";
      Color    : String := Default_Fg;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity);
   --  Change the indentation level for traces with the same output stream.
   --  This is so that traces that result from other subprograms be slightly
   --  indented, so as to make the output more readable. The output would for
   --  instance look like:
   --       [HANDLE1] Procedure 1
   --          [HANDLE2] Procedure 2
   --       [HANDLE1] End of Procedure 1
   --  If Handle and Msg are specified, a message is output on that handle to
   --  explain the change of indentation. The message is only displayed if the
   --  handle is active, but the indentation is always changed.

   function Count
      (Handler : not null access Trace_Handle_Record'Class) return Natural;
   --  Return the number of times that Trace was called on the handler. This
   --  count is only incremented when Handler is active.

   procedure Set_Active
      (Handle : not null access Trace_Handle_Record'Class; Active : Boolean)
      with Inline;
   --  Override the activation status for Handle.
   --  When not Active, the Trace function will do nothing.

   function Is_Active
      (Handle : not null access Trace_Handle_Record'Class) return Boolean
      with Inline;

   function Active
      (Handle : not null access Trace_Handle_Record'Class) return Boolean
      is (Debug_Mode and then Is_Active (Handle)) with Inline;
   --  Return True if traces for Handle are actived.
   --  This function can be used to avoid the evaluation of complex
   --  expressions in case traces are not actived, as in the following
   --  code:
   --     if Active (Handle) then
   --        Trace (Handle, Message & Expensive_Computation);
   --     end if;
   --
   --  Is_Active will check the flag on the trace handle, which is fast but
   --  can only be done dynamically. Active, on the other hand, also checks
   --  the Debug_Mode flag statically, so that if you have disable debugging
   --  altogether, the code will not even be inserted in the object code by
   --  the compiler.

   ------------
   -- Blocks --
   ------------

   type Block_Trace_Handle (<>) is limited private
      with Warnings => Off;
   subtype Block_Logger is Block_Trace_Handle;
   --  The aspect avoids warnings on unused instances, yet allows code to
   --  manipulate those instances when needed (which a "Unused=>True" would
   --  not)

   function Create
      (Handle   : Logger;
       Message  : String := "";
       Location : String := GNAT.Source_Info.Source_Location;
       Entity   : String := GNAT.Source_Info.Enclosing_Entity;
       Color    : String := Default_Fg)
      return Block_Logger;
   --  An object used to trace execution of blocks.
   --  This is a controlled object, which you should create first in your
   --  subprogram, and that will automatically finalize itself when the
   --  subprogram exists. For instance:
   --       Me : constant Logger := Create ("PKG");
   --       procedure Foo (A : Integer) is
   --          Block_Me : constant Block_Logger := Create (Me);
   --       begin
   --          Trace (Me, "A=" & A'Img);
   --          if A > 1 then
   --             Foo (A - 1);
   --          end if;
   --       end Foo;
   --       Foo (2);
   --
   --  which will automatically display in the traces :
   --      [PKG] Entering Foo:pkg.adb:5
   --         [PKG] A= 2
   --         [PKG] Entering Foo:pkg.adb:5
   --            [PKG] A=1
   --         [PKG] Leaving Foo:pkg.adb:5
   --      [PKG] Leaving Foo:pkg.adb:5
   --
   --  Note the use of "with Unreferenced" in the above example (which could
   --  be replaced with a pragma Unreferenced). This is to avoid warnings from
   --  the compiler that the variable is unused, and is only necessary if you
   --  are compiling with -gnatwa or -gnatwm.
   --
   --  Message can be used to display extra information. For efficiency reaons,
   --  it is not recommended to build the string dynamically to display the
   --  parameter of the enclosing subprograms, or perhaps as:
   --
   --       procedure Foo (A, B, C : Integer) is
   --          Block_Me : constant Block_Logger := Create
   --             (Me, (if Active (Me) then A'Img & B'Img & C'Img else ""));
   --       begin
   --          null;
   --       end Foo;
   --
   --  so that the string is only built if the trace is active.
   --
   --  If the subprogram exits with an exception, no trace of the exception
   --  is displayed, you should still have an explicit exception handler if
   --  you want to Trace that exception. Of course, the "Leaving" message will
   --  be properly displayed.

   -------------
   -- Streams --
   -------------

   type Typical_Msg_Size is mod 128;
   for Typical_Msg_Size'Size use 8;
   package Msg_Strings is new GNATCOLL.Strings_Impl.Strings (Typical_Msg_Size);
   --  We assume that most messages (including decorators) will be less than
   --  this number of characters, and optimize the string creation for this.
   --  But we still support larger messages, at a cost of one memory
   --  allocation which slows things down a bit.

   type Trace_Stream_Record is abstract tagged limited private;
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
     (Stream     : in out Trace_Stream_Record;
      Str        : Msg_Strings.XString) is abstract;
   --  Outputs a whole line to the stream.
   --  Str always ends up with a trailing newline.
   --  The stream needs to take appropriate lock or other synchronization
   --  mechanism to avoid mixing multiple lines of output. This lets each
   --  stream have its own lock, rather than a global lock, which improves
   --  the throughput.

   procedure Close (Stream : in out Trace_Stream_Record);
   --  Close the stream

   function Supports_Color (Stream : Trace_Stream_Record) return Boolean
      is (True);
   function Supports_Time  (Stream : Trace_Stream_Record) return Boolean
      is (True);
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

   procedure Set_Default_Stream
      (Name        : String;
       Config_File : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File);
   --  Set Name as the default stream.
   --  See Register_Stream_Factory for a list of valid names. The name can be
   --  prefixed with ">>" to append to that stream.
   --  An optional '>' is also allowed, although it is implicit if not
   --  specified.
   --  The Config_File is used to resolve a relative path name when
   --  needed.

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
   --  If this handle is actived, two counters are associated with each output
   --  trace: one of them is unique for the handle, the other is unique in the
   --  whole application life. These can for instance be used to set
   --  conditional breakpoints for a specific trace (break on traces.Log or
   --  traces.Trace, and check the value of Handle.Count

   --  "DEBUG.MEMORY"
   --  This decorator will show the size of resident memory for the
   --  application, as well as the peek size. This takes into account memory
   --  allocated from any language, C, Ada,.. and is queries from the
   --  operating system).
   --  It also shows a ">" or "<" to indicate whether memory use increased or
   --  not.

   --  "DEBUG.ADA_MEMORY"
   --  This is similar to DEBUG.MEMORY, but only displays memory allocated
   --  from Ada (provided you have setup GNATCOLL.Memory to become the default
   --  allocator for your application).

   --  "DEBUG.FINALIZE_TRACES"   (default: active)
   --  If deactivated, the trace handles will never be freed when the program
   --  is finalized by the compiler. This is mostly for debugging purposes.

   --  "DEBUG.SPLIT_LINES"  (default: true)
   --  Whether long messages should be split at each ASCII.LF character. When
   --  we do this, the trace handle name and decorators are replicated at the
   --  beginning of each followup line. This results in a slow down.

   type Trace_Decorator_Record is new Trace_Handle_Record with private;
   type Trace_Decorator is access all Trace_Decorator_Record'Class;

   procedure Start_Of_Line
     (Self            : in out Trace_Decorator_Record;
      Msg             : in out Msg_Strings.XString;
      Is_Continuation : Boolean) is null;
   --  Called at the start of each line of the message. This procedure
   --  should modify Msg to Append extra information to it, if needed.
   --  GNATCOLL.Traces will then append the indentation for each line
   --  automatically, see Increase_Indent and Decrease_Indent.
   --  You can override this procedure to display a timestamp aligned at the
   --  beginning of the line, for instance.

   procedure Before_Message
     (Self   : in out Trace_Decorator_Record;
      Handle : not null Logger;
      Msg    : in out Msg_Strings.XString) is null;
   procedure After_Message
     (Self   : in out Trace_Decorator_Record;
      Handle : not null Logger;
      Msg    : in out Msg_Strings.XString) is null;
   --  You can override either of these two procedures to create your own
   --  decorators to specific trace handles (ie additional information) each
   --  time some message is logged. These functions are only called for the
   --  handles passed to Add_Global_Decorator.
   --
   --  When displayed in the log, a line looks like:
   --
   --  <prefix_decorator>[HANDLE_NAME] <precorator> MESSAGE </postdecorator>
   --
   --  The prefix decorator is in charge of displaying blank spaces to
   --  indent the line (see Increase_Indent and Decrease_Indent). But you
   --  can also use it to display other pieces of information (like a
   --  timestamp if you always want them aligned for instance).
   --  Any global decorator will be called before the indentation (so at
   --  column 1).
   --  Indent is the indentation level (1, 2, 3,...). This isn't the
   --  number of columns to indent.
   --
   --  Only the prefix_decorator is called on continuation lines (when a
   --  message doesn't fit on a single line).

   procedure Add_Global_Decorator
      (Decorator : not null access Trace_Decorator_Record'Class;
       Name      : String);
   --  Register a global decorator that will apply to all existing
   --  Trace_Handle. The decorator only has an effect when it is active.
   --  Here is an example:
   --     type My_Decorator is new Trace_Handle_Record with null record;
   --     overriding procedure Before_Message
   --        (Self    : in out My_Decorator;
   --         Handle  : not null Logger;
   --         Message : in out Msg_Strings.XString)  is
   --     begin
   --        Append (Message, "Some info");
   --     end Before_Message;
   --
   --     Add_Global_Decorator (new My_Decorator, "MY_DECO");
   --
   --  And then you can use your configuration file as usual to activate or
   --  deactivate the "MY_DECO" handle.
   --
   --  A decorator is disabled by default (when it is registered via this
   --  procedure). To active, you can either do this in the configuration
   --  file, with the usual:
   --      MY_DECO=yes
   --  or in the code, as:
   --      Set_Active (Create ("MY_DECO"), True);

private
   type Trace_Stream_Record is abstract tagged limited record
      Name          : GNAT.Strings.String_Access;
      Next          : Trace_Stream;
      Indentation   : aliased Atomic_Counter := 0;
      --  Current indentation for stream
   end record;
   --  Name is the full name including the arguments, for instance "file:foo"
   --  if the user has defined a stream called "file" with a parameter "foo"

   type Block_Trace_Handle is new Ada.Finalization.Limited_Controlled with
   record
      Me            : Logger;
      Loc           : GNAT.Strings.String_Access;
   end record;
   overriding procedure Finalize (Self : in out Block_Logger);

   type Trace_Handle_Record is tagged limited record
      Next          : Logger;  --  linked list
      Name          : GNAT.Strings.String_Access;
      Timer         : Ada.Calendar.Time;
      Stream        : Trace_Stream;  --  null for default stream

      Exception_Handle : Logger;
      --  The handle used when calling Trace and passing an exception
      --  occurrence. This has  Name & ".EXCEPTIONS" as a name, and is created
      --  the first time it is needed.

      Count             : aliased Atomic_Counter;
      Finalize          : Boolean;
      Active            : Boolean;
      Forced_Active     : Boolean;
      Stream_Is_Default : Boolean;

      With_Colors       : Boolean := False;
      With_Time         : Boolean := False;
      --  Compute values, from the stream and corresponding settings. These
      --  are used to avoid dispatching calls in Log.
   end record;
   pragma Pack (Trace_Handle_Record);
   --  If Forced_Active is true, then the Active status shouldn't be impacted
   --  by a '+' in the configuration file

   type Trace_Decorator_Record is new Trace_Handle_Record with null record;

end GNATCOLL.Traces;
