------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2021-2022, AdaCore                   --
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

--  Provides a low-level portable API to iterate on directories content. The
--  main advantage of this implementation over the default one in the GNAT
--  runtime is that it returns both the filename and the "stat" information
--  for each entry in a given directory. On systems such as Windows this
--  reduce significantly the cost of file system operations as the Win32 API
--  can achieve this with one system call (the equivalent to readdir).

with Ada.Strings.UTF_Encoding;
with GNATCOLL.OS.Stat;
with GNATCOLL.OS.Dir_Types;
with Ada.Calendar; use Ada.Calendar;

package GNATCOLL.OS.Dir is

   package UTF8 renames Ada.Strings.UTF_Encoding;
   package Stat renames GNATCOLL.OS.Stat;

   type Dir_Handle is private;
   --  Handle on a directory

   type Dir_Entry is private;
   --  Directory entry

   function Open (Path : UTF8.UTF_8_String) return Dir_Handle;
   --  Open a directory at path Path. In case of error OS_Error exception is
   --  raised.

   procedure Close (Handle : in out Dir_Handle);
   --  Close a directory. In case of error OS_Error is raised.

   function Is_Opened (Handle : Dir_Handle) return Boolean;
   --  Return True of the handle is opened.

   function Read
      (Handle          : Dir_Handle;
       Follow_Symlinks : Boolean := True)
      return Dir_Entry;
   --  Read next entry in directory Handle. In case of error OS_Error is
   --  raised. When reaching the end of the entry list a special Dir_Entry E
   --  is returned for which End_Of_Iteration (E) is True. If Follow_Symlinks
   --  is set to True, the stat information stored is the Dir_Entry is the
   --  stat information of the file targeted by the link. Otherwise the stat
   --  information of the link itself is returned.

   function Path (Handle : Dir_Handle) return UTF8.UTF_8_String;
   --  Returned path associated with the handle. If Handle is not opened then
   --  OS_Error is raised.

   function End_Of_Iteration (Self : Dir_Entry) return Boolean;
   --  Return True if the entry represent the end of the iteration. Note that
   --  the entry for which En_Of_Iteration returns True is not a valid entry.
   --  (no name and no attributes are set).

   function Name (Self : Dir_Entry) return UTF8.UTF_8_String;
   --  Return the filename for Self (note that this is just the basename)

   function Path (Dir : Dir_Handle; Self : Dir_Entry) return UTF8.UTF_8_String;
   --  Return the full path for a given entry

   function Attributes (Self : Dir_Entry) return Stat.File_Attributes;
   --  Return the file attributes for Self

   function Is_File (Self : Dir_Entry) return Boolean;
   --  Return True if the file is a regular file

   function Is_Directory (Self : Dir_Entry) return Boolean;
   --  Return True if the file is a directory

   function Is_Symbolic_Link (Self : Dir_Entry) return Boolean;
   --  Return True if the file is a symbolic link

   function Is_Executable (Self : Dir_Entry) return Boolean;
   --  Return True if the file is executable

   function Is_Readable (Self : Dir_Entry) return Boolean;
   --  Return True if the file is readable

   function Is_Writable (Self : Dir_Entry) return Boolean;
   --  Return True if the file is writable

   function Is_Executable_File (Self : Dir_Entry) return Boolean;
   --  Return True if the file is a regular file and is executable

   function Modification_Time (Self : Dir_Entry) return Time;
   --  Return file modification time

   type Process_File is access
      procedure (Dir : Dir_Handle; Element : Dir_Entry);
   --  Function called on each file except directories found by Walk

   type Process_Directory is access
      function (Dir : Dir_Handle; Element : Dir_Entry) return Boolean;
   --  Function called on each directory found by Walk. If the handler returns
   --  True then the directory is also explored. If False then the directory
   --  is skipped.

   type Walk_Error is (OPEN_DIR_ERROR, OTHER_ERROR);

   type Process_Error is access
      procedure (Dir     : Dir_Handle;
                 Element : Dir_Entry;
                 Error   : Walk_Error;
                 Msg     : String);
   --  Function called in case an error occurs during a call to walk

   procedure Raise_Exception_On_Error
      (Dir     : Dir_Handle;
       Element : Dir_Entry;
       Error   : Walk_Error;
       Msg     : String);
   --  Raise exception in case of error

   Ignore         : constant Process_Error := null;
   Raise_OS_Error : constant Process_Error := Raise_Exception_On_Error'Access;

   procedure Walk
      (Path            : UTF8.UTF_8_String;
       File_Handler    : Process_File;
       Dir_Handler     : Process_Directory := null;
       Max_Depth       : Positive          := 256;
       On_Error        : Process_Error     := Ignore;
       Follow_Symlinks : Boolean           := False);
   --  Explore recursively a directory Path. For each entry found, call
   --  Dir_Handler if the entry is a directory and File_Handler otherwise.
   --  if a call to Dir_Handler returns True then Walk explores that
   --  subdirectory. Otherwise the subdirectory is skipped. Note that if a null
   --  Dir_Handler is passed then Walk behaves as if Dir_Handler always returns
   --  True (and thus explore all subdirectories).
   --
   --  Max_Depth limits the depth of the exploration. For example a Max_Depth
   --  of 1 is equivalent to a non recursive call as it returns only direct
   --  entries of Path.
   --
   --  On_Error handler is called on each error during the exploration of Path.
   --  Two default handlers are already implemented: Ignore and Raise_OS_Error.
   --
   --  If Follow_Symlinks is True, each time a symbolic link is encountered the
   --  target of the link is returned. If False, the link itself is returned.
   --  Note that setting Follow_Symlink to True might lead in theory to
   --  infinite loop. In our case the Max_Depth parameter provides a limit.

private

   PATH_MAX : constant Integer := 4096;
   NAME_MAX : constant Integer := 1024;

   type Dir_Handle is record
      Handle    : GNATCOLL.OS.Dir_Types.OS_Dir_Handle;
      Is_Opened : Boolean := False;
      Path_Last : Integer;
      Path      : UTF8.UTF_8_String (1 .. PATH_MAX);
   end record;

   type Dir_Entry is record
      Info        : Stat.File_Attributes;
      Name_Last   : Integer;
      Name_Buffer : UTF8.UTF_8_String (1 .. NAME_MAX);
   end record;

end GNATCOLL.OS.Dir;
