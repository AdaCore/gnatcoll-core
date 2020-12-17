------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

--  Low level API to Operating Systems Filesystem

with Ada.Strings.UTF_Encoding;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package GNATCOLL.OS.FS is

   package UTF8 renames Ada.Strings.UTF_Encoding;

   type File_Descriptor is private;
   --  A File descriptor

   Standin  : constant File_Descriptor;
   Standout : constant File_Descriptor;
   Standerr : constant File_Descriptor;
   --  File descriptors for standard input output files

   Invalid_FD : constant File_Descriptor;
   --  File descriptor returned when there is an error when opening/creating a
   --  file.

   To_Stdout : constant File_Descriptor;
   --  Used by some functions to indicate that a descriptor is a copy of the
   --  stdout descriptor

   Null_FD   : constant File_Descriptor;
   --  Used by some functions to redirect a file descriptor to the null file
   --  (i.e /dev/null on unix or NUL on windows)

   type Open_Mode is (Read_Mode, Write_Mode, Append_Mode);

   function Open
      (Path : UTF8.UTF_8_String;
       Mode : Open_Mode := Read_Mode)
      return File_Descriptor;
   --  Open a file located at Path.
   --
   --  If the file cannot be opened the function returned Invalid_FD.
   --  In Write_Mode and Append_Mode if the file does not exist it is created.
   --  File is opened in "close on exec" mode (i.e: the file descriptor is not
   --  inherited by a child process)

   procedure Open_Pipe
      (Pipe_Input  : out File_Descriptor;
       Pipe_Output : out File_Descriptor);
   --  Open a pipe. Data can be written on Pipe_Input and then read on
   --  Pipe_Output. OS_Error can be raised in case of error. Note that the pipe
   --  is opened with "close on exec" mode.

   procedure Set_Close_On_Exec
     (FD            : File_Descriptor;
      Close_On_Exec : Boolean);
   --  Control whether a file descriptor is inherited by subprocesses. In
   --  case of error, OS_Error can be raised.

   procedure Close (FD : File_Descriptor);
   --  Close a file descriptor.

   function Is_Console (FD : File_Descriptor) return Boolean;
   --  Return True if the FD correspond to a console.

   function Read (FD : File_Descriptor; Buffer : in out String) return Integer;
   --  Read data from FD and put it in Buffer. The call is blocking and
   --  end-of-file is reached when Read returns 0, otherwise the return value
   --  is the number of bytes read.

   function Read
      (FD          : File_Descriptor;
       Buffer_Size : Positive := 4096)
      return Unbounded_String;
   --  Read full file content.

   function Read
      (FD          : File_Descriptor;
       Buffer_Size : Positive := 4096)
      return String;
   --  Read full file content. As String might rely on stack, usually it's
   --  preferable to use the variant that returns an Unbounded_String.

   function Write
      (FD : File_Descriptor; Buffer : String) return Integer;
   --  Write Buffer content to FD. The call is blocking. OS_Error is raised is
   --  the write operation failed. Write return the number of bytes effectively
   --  written (the result might be inferior to Buffer'Length if interrupted
   --  by a signal handler, not enough disk space, ...).

   procedure Write (FD : File_Descriptor; Buffer : String);
   --  Write Buffer content to FD. OS_Error is raised if write fails or is not
   --  complete.

private

   type File_Descriptor is new Integer;

   Standin    : constant File_Descriptor := 0;
   Standout   : constant File_Descriptor := 1;
   Standerr   : constant File_Descriptor := 2;
   Invalid_FD : constant File_Descriptor := -1;
   To_Stdout  : constant File_Descriptor := -2;
   Null_FD    : constant File_Descriptor := -3;

end GNATCOLL.OS.FS;
