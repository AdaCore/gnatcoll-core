------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                   Copyright (C) 2021-2023, AdaCore                       --
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

--  Declare types and bindings to Win32 process related APIs

with System;

package GNATCOLL.OS.Win32.Process is

   type STARTUPINFO is record
      cb            : DWORD  := 0;
      Reserved      : LPWSTR := LPWSTR (System.Null_Address);
      Desktop       : LPWSTR := LPWSTR (System.Null_Address);
      Title         : LPWSTR := LPWSTR (System.Null_Address);
      X             : DWORD  := 0;
      Y             : DWORD  := 0;
      XSize         : DWORD  := 0;
      YSize         : DWORD  := 0;
      XCountChars   : DWORD  := 0;
      YCountChars   : DWORD  := 0;
      FillAttribute : DWORD  := 0;
      Flags         : DWORD  := 0;
      ShowWindow    : WORD   := 0;
      Reserved2     : WORD   := 0;
      Reserved3     : LPVOID := LPVOID (System.Null_Address);
      StdInput      : HANDLE := NULL_HANDLE;
      StdOutput     : HANDLE := NULL_HANDLE;
      StdError      : HANDLE := NULL_HANDLE;
   end record
   with Convention => C_Pass_By_Copy;

   STARTF_USESTDHANDLES : constant DWORD := 16#00000100#;
   SW_HIDE : constant WORD := 0;

   type LPSTARTUPINFO is access all STARTUPINFO;

   type PROCESS_INFORMATION is record
      Process   : HANDLE;
      Thread    : HANDLE;
      ProcessId : DWORD;
      ThreadId  : DWORD;
   end record
   with Convention => C_Pass_By_Copy;

   type LPPROCESS_INFORMATION is access all PROCESS_INFORMATION;

   function WaitForSingleObject
      (Object       : HANDLE;
       Milliseconds : DWORD)
      return DWORD
   with Import => True,
        Convention => Stdcall,
        External_Name => "WaitForSingleObject";

   function WaitForMultipleObjects
      (Count        : DWORD;
       Handles      : LPVOID;
       WaitForAll   : BOOL;
       Milliseconds : DWORD)
      return DWORD
   with Import        => True,
        Convention    => Stdcall,
        External_Name => "WaitForMultipleObjects";

   function ThreadedWaitForMultipleObjects
      (Count        : DWORD;
       Handles      : LPVOID;
       WaitForAll   : BOOL;
       Milliseconds : DWORD)
      return Integer
   with Import        => True,
        Convention    => C,
        External_Name => "__gnatcoll_wait_for_multiple_objects";
   --  Alternative to WaitForMultipleObjects that can wait for up to 4096
   --  handles instead of 64. Note that the function returns -1 in case of
   --  error, -2 in case of timeout, and in case of success the index in
   --  Handles of the signaled or abandoned object.

   INFINITE         : constant DWORD := 16#ffffffff#;
   WAIT_OBJECT_0    : constant DWORD := 16#00000000#;
   WAIT_ABANDONED_0 : constant DWORD := 16#00000080#;
   WAIT_TIMEOUT     : constant DWORD := 16#00000102#;
   WAIT_FAILED      : constant DWORD := 16#FFFFFFFF#;

   function GetExitCodeProcess
      (Process  : HANDLE;
       ExitCode : out DWORD)
      return BOOL
   with Import => True,
        Convention => Stdcall,
        External_Name => "GetExitCodeProcess";

   function CloseHandle (Object : HANDLE) return BOOL
   with Import => True,
        Convention => Stdcall,
        External_Name => "CloseHandle";

   subtype Process_Creation_Flags is DWORD;

   function CreateProcess
     (ApplicationName    : C_WString;
      CommandLine        : C_WString;
      ProcessAttributes  : LPSECURITY_ATTRIBUTES;
      ThreadAttributes   : LPSECURITY_ATTRIBUTES;
      bInheritHandles    : BOOL;
      CreationFlags      : Process_Creation_Flags;
      Environment        : C_WString;
      CurrentDirectory   : C_WString;
      Startup            : in out STARTUPINFO;
      ProcessInformation : in out PROCESS_INFORMATION)
      return BOOL
   with Import => True,
        Convention => Stdcall,
        External_Name => "CreateProcessW";

   --  Priorities
   IDLE_PRIORITY_CLASS         : constant Process_Creation_Flags := 16#40#;
   BELOW_NORMAL_PRIORITY_CLASS : constant Process_Creation_Flags := 16#4000#;
   NORMAL_PRIORITY_CLASS       : constant Process_Creation_Flags := 16#20#;
   ABOVE_NORMAL_PRIORITY_CLASS : constant Process_Creation_Flags := 16#8000#;
   HIGH_PRIORITY_CLASS         : constant Process_Creation_Flags := 16#80#;

   --  Other creation flags
   CREATE_UNICODE_ENVIRONMENT  : constant Process_Creation_Flags := 16#400#;

   subtype PROCESS_INFORMATION_CLASS is unsigned;
   ProcessBasicInformation     : constant PROCESS_INFORMATION_CLASS := 0;
   ProcessDebugPort            : constant PROCESS_INFORMATION_CLASS := 7;
   ProcessWow64Information     : constant PROCESS_INFORMATION_CLASS := 26;
   ProcessImageFileName        : constant PROCESS_INFORMATION_CLASS := 27;
   ProcessBreakOnTermination   : constant PROCESS_INFORMATION_CLASS := 29;
   ProcessSubsystemInformation : constant PROCESS_INFORMATION_CLASS := 75;

   subtype KPRIORITY is LONG;

   type PROCESS_BASIC_INFORMATION is record
      ExitStatus              : NTSTATUS;
      PebBaseAddress          : LPVOID;
      AffinityMask            : ULONG_PTR;
      BasePriority            : KPRIORITY;
      UniqueProcessId         : ULONG_PTR;
      IheritedUniqueProcessId : ULONG_PTR;
   end record
   with Convention => C_Pass_By_Copy;

   function NtQueryInformationProcess
     (ProcessHandle            : HANDLE;
      ProcessInformationClass  : PROCESS_INFORMATION_CLASS;
      ProcessInformation       : System.Address;
      ProcessInformationLength : ULONG;
      ReturnLength             : out ULONG)
     return NTSTATUS
   with Import => True,
        Convention => Stdcall,
        External_Name => "NtQueryInformationProcess";

end GNATCOLL.OS.Win32.Process;
