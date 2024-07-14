------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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

--  Portable process API

with Ada.Strings;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;
with GNATCOLL.OS.FS;
with GNATCOLL.OS.Process_Types;

package GNATCOLL.OS.Process is

   package Process_Types renames GNATCOLL.OS.Process_Types;
   package UTF8          renames Ada.Strings.UTF_Encoding;
   package FS            renames GNATCOLL.OS.FS;

   -----------------------------
   --  High level process API --
   -----------------------------

   type Process_Handle is new Process_Types.Process_Handle;
   --  Private type that map to a process "handle", the type in fact maps to
   --  different type of object depending on the system. On Unix system it will
   --  be usually a process id and on Windows system a handle.

   Invalid_Handle : constant Process_Handle :=
      Process_Handle (Process_Types.Invalid_Handle);

   type Process_Array is array (Natural range <>) of Process_Handle;

   type Process_State is
     (RUNNING,
      WAITABLE,
      TERMINATED);
   --  When a process is spawned it goes into RUNNING state. Once the process
   --  calls exit it goes into the WAITABLE state (i.e: this means call to
   --  Wait will succeed and will not block). Finally once the process has
   --  disappeared from the system (after a call to Wait) the state is
   --  TERMINATED.

   type Priority_Class is
     (INHERIT,
      IDLE,
      BELOW_NORMAL,
      NORMAL,
      ABOVE_NORMAL,
      HIGH);
   --  Process priorities.
   --
   --  Note that on Unix systems, Priority setting in the following APIs might
   --  not always be honored. When a child process priority cannot be set, no
   --  error is raised. The priority of the child process is then set to the
   --  parent process priority.

   function Equivalent_Variables
      (Left, Right : UTF8.UTF_8_String) return Boolean;
   --  Internal function used by Environment_Dict to decide if two environment
   --  variable names correspond to the same environment variable.

   function Variable_Name_Hash (Key : UTF8.UTF_8_String)
      return Ada.Containers.Hash_Type;
   --  Hash function used by Environment_Dicts.

   package Env_Dicts is
      new Ada.Containers.Indefinite_Hashed_Maps
         (UTF8.UTF_8_String,
          UTF8.UTF_8_String,
          Hash            => Variable_Name_Hash,
          Equivalent_Keys => Equivalent_Variables);
   use Env_Dicts;
   --  Environment dict. If your application already handles this structure
   --  internally, we can avoid this intermediate dictionary by using the
   --  low-level version of the API.

   package Arg_Lists is
      new Ada.Containers.Indefinite_Vectors
         (Natural,
          UTF8.UTF_8_String);
   use Arg_Lists;
   --  Process arguments. As for Environment, another version of the API
   --  provides lower level API in which you can avoid the use of that
   --  dynamic structure.

   subtype Argument_List is Arg_Lists.Vector;
   --  Command line arguments.
   --
   --  In subsequent APIs, it's assumed that Argument_List passed have at
   --  least one element (the command name) and that the first element is not
   --  the empty string. An OS_Error exception is raised when passing
   --  Argument_List parameter that do not respect these constraints.

   subtype Environment_Dict is Env_Dicts.Map;
   --  Environment to pass to the child process

   function Start
      (Args     : Argument_List;
       Cwd      : UTF8.UTF_8_String  := "";
       Stdin    : FS.File_Descriptor := FS.Standin;
       Stdout   : FS.File_Descriptor := FS.Standout;
       Stderr   : FS.File_Descriptor := FS.Standerr;
       Priority : Priority_Class     := INHERIT)
      return Process_Handle;

   function Start
      (Args        : Argument_List;
       Env         : Environment_Dict;
       Cwd         : UTF8.UTF_8_String  := "";
       Stdin       : FS.File_Descriptor := FS.Standin;
       Stdout      : FS.File_Descriptor := FS.Standout;
       Stderr      : FS.File_Descriptor := FS.Standerr;
       Priority    : Priority_Class     := INHERIT;
       Inherit_Env : Boolean            := False)
      return Process_Handle;
   --  Start a process (non-blocking) and return a handle on it
   --
   --  In case the process cannot be spawned, OS_Error can be raised. In some
   --  cases the process can be created but the command line cannot be
   --  executed. In that case the exit status returned by Wait will be 127.
   --
   --  Args represent the command line to spawn. Note that the first element
   --  is the program to launch and subsequent ones the arguments to that
   --  program.
   --
   --  If no Env is passed then environment is inherited from the parent
   --  process, otherwise environment is overridden with content of Env. Note
   --  that in order to improve usability, on Windows when empty Env is
   --  passed then SYSTEMROOT and SYSTEMDRIVE are automatically added to the
   --  environment using the current process value, as without these
   --  variables process spawning fails. If Env is passed and Inherit_Env is
   --  set to True, then the environment passed to the process is the parent
   --  environment on top of which the content of Env is applied.
   --
   --  If Cwd is not empty then the process will be executed in that directory.
   --
   --  Stdin, Stdout and Stderr are file descriptors to stdin, stdout and
   --  stderr respectively. Note that you can set Stderr to FS.To_Stdout
   --  to redirect stderr to stdout.
   --
   --  Priority defines the process priority. By default priority is
   --  inherited from the parent process.

   function Wait (H : Process_Handle) return Integer;
   --  Wait for a process to end, and return its exit code

   function State (H : Process_Handle) return Process_State;
   --  Return the process state

   INFINITE_TIMEOUT : constant Duration := 4_294_967.0;
   --  Beyond that value timeouts are considered to be infinite. This
   --  corresponds roughly to a bit more than 49 days. The value comes from
   --  the fact that on Windows the max timeout that can be used is
   --  2 ** 16 - 1 (4_294_967_294) milliseconds. Using that special value
   --  rather than a special negative value, ensures good arithmetic properties
   --  and protection against overflows.

   WAIT_NO_PROCESS : constant Integer := -1;
   WAIT_TIMEOUT    : constant Integer := -2;

   function Wait_For_Processes
     (Processes : Process_Array;
      Timeout   : Duration := INFINITE_TIMEOUT)
     return Integer;
   --  Wait for multiple processes and return the index of the first process
   --  for which state is WAITABLE. If the timeout is reached then WAIT_TIMEOUT
   --  is returned. If there is no process RUNNING then WAIT_NO_PROCESS is
   --  returned.
   --
   --  If INFINITE_TIMEOUT or greater value is used as Timeout value then the
   --  function waits indefinitely.
   --
   --  Unix limitations: there is no maximum length for Processes,
   --  and the number of simultaneous calls per process to that function is
   --  256.
   --
   --  Windows limitation: the maximum length for Processes is 4096. Note that
   --  beyond 64 processes some foreign native threads are used.

   function Wait_For_Processes
     (Processes : Process_Array;
      Timeout   : Duration := INFINITE_TIMEOUT)
      return Process_Handle;
   --  Same as previous function except that the Handle is returned instead of
   --  the index in Processes. Note that if the status was WAIT_TIMEOUT or
   --  WAIT_NO_PROCESS then Invalid_Handle is returned.

   function Run
      (Args        : Argument_List;
       Env         : Environment_Dict;
       Cwd         : UTF8.UTF_8_String  := "";
       Stdin       : FS.File_Descriptor := FS.Standin;
       Stdout      : FS.File_Descriptor := FS.Standout;
       Stderr      : FS.File_Descriptor := FS.Standerr;
       Priority    : Priority_Class     := INHERIT;
       Inherit_Env : Boolean            := False)
      return Integer;

   function Run
      (Args     : Argument_List;
       Cwd      : UTF8.UTF_8_String := "";
       Stdin    : FS.File_Descriptor := FS.Standin;
       Stdout   : FS.File_Descriptor := FS.Standout;
       Stderr   : FS.File_Descriptor := FS.Standerr;
       Priority : Priority_Class     := INHERIT)
      return Integer;
   --  Start a process and wait for its termination.
   --
   --  The function takes the same arguments as Start but returns an exit
   --  status.

   function Run
     (Args              : Argument_List;
      Cwd               : String             := "";
      Stdin             : FS.File_Descriptor := FS.Standin;
      Stderr            : FS.File_Descriptor := FS.Standerr;
      Priority          : Priority_Class     := INHERIT;
      Universal_Newline : Boolean            := False;
      Strip             : Boolean            := False;
      Status            : out Integer)
      return Ada.Strings.Unbounded.Unbounded_String;

   function Run
     (Args              : Argument_List;
      Env               : Environment_Dict;
      Cwd               : String             := "";
      Stdin             : FS.File_Descriptor := FS.Standin;
      Stderr            : FS.File_Descriptor := FS.Standerr;
      Priority          : Priority_Class     := INHERIT;
      Universal_Newline : Boolean            := False;
      Strip             : Boolean            := False;
      Inherit_Env       : Boolean            := False;
      Status            : out Integer)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Same as Run but in addition the standard output is captured and returned
   --
   --  The function takes two additional parameters:
   --  Universal_Newline: when True, sequences of CR + LF are transformed into
   --      LF
   --  Strip: when True, both leading and trailing CR, LF, HT and spaces are
   --      stripped from the output

   ---------------------------
   -- Low level process API --
   ---------------------------

   --  Lower level API (still portable) that can be used when alternate
   --  structures are holding the arguments and the environment

   function Start
     (Args     : Process_Types.Arguments;
      Env      : Process_Types.Environ;
      Cwd      : UTF8.UTF_8_String  := "";
      Stdin    : FS.File_Descriptor := FS.Standin;
      Stdout   : FS.File_Descriptor := FS.Standout;
      Stderr   : FS.File_Descriptor := FS.Standerr;
      Priority : Priority_Class     := INHERIT)
      return Process_Handle;
   --  Like previous declaration of Start except that lower level structures
   --  are used for Args and Env.

   function Run
     (Args     : Process_Types.Arguments;
      Env      : Process_Types.Environ;
      Cwd      : UTF8.UTF_8_String  := "";
      Stdin    : FS.File_Descriptor := FS.Standin;
      Stdout   : FS.File_Descriptor := FS.Standout;
      Stderr   : FS.File_Descriptor := FS.Standerr;
      Priority : Priority_Class     := INHERIT)
      return Integer;
   --  Like previous declaration of Run except that lower level structures
   --  are used for Args and Env.

   function Run
     (Args              : Process_Types.Arguments;
      Env               : Process_Types.Environ;
      Cwd               : UTF8.UTF_8_String  := "";
      Stdin             : FS.File_Descriptor := FS.Standin;
      Stderr            : FS.File_Descriptor := FS.Standerr;
      Priority          : Priority_Class     := INHERIT;
      Universal_Newline : Boolean            := False;
      Strip             : Boolean            := False;
      Status            : out Integer)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Like previous declaration of Run except that lower level structures
   --  are used for Args and Env.

end GNATCOLL.OS.Process;
