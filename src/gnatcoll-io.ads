------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2015, AdaCore                     --
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

with Ada.Calendar;
with GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.VFS_Types; use GNATCOLL.VFS_Types;

private package GNATCOLL.IO is

   type Item_Type is
     (Unknown,
      --  File is not determined
      File,
      --  Regular file
      Directory
      --  Directory
     );
   --  Item_Type is used to cache the calls to Is_Regular_File or Is_Directory
   --  that can be pretty time consuming, and that are performed pretty often.

   type File_Record is abstract tagged record
      Ref_Count  : Natural := 0;
      Full       : FS_String_Access;
      --  The file's full path

      Normalized : FS_String_Access;
      --  The file's normalized form ('..' and '.' directories removed)

      Normalized_And_Resolved : FS_String_Access;
      --  The normalized form with resolved symlinks.
      --  This points to the same value as Normalized if these have the same
      --  value.

      Kind       : Item_Type := Unknown;
      --  The kind of file represented by this object
   end record;

   type File_Access is access all File_Record'Class;
   type File_Array is array (Natural range <>) of File_Access;

   procedure Ref (File : File_Access);
   procedure Unref (File : in out File_Access);
   procedure Destroy (File : in out File_Record);

   function Dispatching_Create
     (Ref : not null access File_Record;
      Full_Path : FS_String) return File_Access is abstract;
   --  Create a new file using the same tagged type is Ref

   function To_UTF8
     (Ref : not null access File_Record;
      Path : FS_String) return String is abstract;
   function From_UTF8
     (Ref : not null access File_Record;
      Path : String) return FS_String is abstract;
   --  Translate a path to/from UTF8 encoded strings, according to the
   --  Filesystem's charset.

   function Is_Local (File : File_Record) return Boolean is abstract;
   --  Tell if IO denotes a local file or directory

   function Get_FS
     (File : not null access File_Record) return FS_Type is abstract;
   --  Return the kind of FS the file is on

   procedure Resolve_Symlinks (File : not null access File_Record) is abstract;
   --  Resolve all potential symlinks present in the IO path.
   --  Does nothing if this computation has already been done.

   ----------------------
   -- Queries on files --
   ----------------------

   function Is_Regular_File
     (File : not null access File_Record) return Boolean is abstract;
   --  Return True if Local_Full_Name exists on the remote host

   function Size
     (File : not null access File_Record) return Long_Integer is abstract;
   --  Return the size of the file in bytes.

   function Is_Directory
     (File : not null access File_Record) return Boolean is abstract;
   --  Return True if File is in fact a directory

   function Is_Symbolic_Link
     (File : not null access File_Record) return Boolean is abstract;
   --  Whether the file is a symbolic link

   function File_Time_Stamp
     (File : not null access File_Record) return Ada.Calendar.Time is abstract;
   --  Return the timestamp for this file.
   --  If the Connection doesn't support this operation, or the file
   --  doesn't exists, it should return a date of No_Time, so as to force, when
   --  possible, a read operation from the caller.

   function Is_Writable
     (File : not null access File_Record) return Boolean is abstract;
   --  Return True if File is writable

   procedure Set_Writable
     (File  : not null access File_Record;
      State : Boolean) is abstract;
   --  If Writable is True, make the file writable, otherwise make the file
   --  unwritable.

   procedure Set_Readable
     (File  : not null access File_Record;
      State : Boolean) is abstract;
   --  If Readable is True, make the file readable, otherwise make the file
   --  unreadable.

   ----------------------
   --  File operations --
   ----------------------

   procedure Rename
     (From    : not null access File_Record;
      Dest    : not null access File_Record;
      Success : out Boolean) is abstract;
   --  Rename From_Local_Name on the host to To_Local_Name on the same host.
   --  Return False if the renaming could not be performed.

   procedure Copy
     (From    : not null access File_Record;
      Dest    : FS_String;
      Success : out Boolean) is abstract;
   --  Copy a file into another one.
   --  To_Local_Name can be the name of the directory in which to copy the
   --  file, or the name of a file to be created.

   procedure Delete
     (File    : not null access File_Record;
      Success : out Boolean) is abstract;
   --  Sends host a delete command for file

   function Read_Whole_File
     (File : not null access File_Record) return GNAT.Strings.String_Access
      is abstract;
   --  Return the contents of an entire file.
   --  If the file cannot be found, return null.
   --  The caller is responsible for freeing the returned memory.
   --  No special encoding/decoding for charsets is done on the file.

   procedure Open_Write
     (File    : not null access File_Record;
      Append  : Boolean := False;
      FD      : out GNAT.OS_Lib.File_Descriptor) is abstract;
   --  Opens a file for writing. Return a file descriptor used to actually
   --  write.
   --  /!\ Do not call close directly on FD, but use the method below instead.

   procedure Close
     (File    : not null access File_Record;
      FD      : GNAT.OS_Lib.File_Descriptor;
      Success : out Boolean) is abstract;
   --  Closes FD and actually flushes the content to File if needed

   procedure Copy_File_Permissions
      (From, To : not null access File_Record;
       Success  : out Boolean) is abstract;
   --  Copy all permissions (read, write, exec) from one file to the other,
   --  so that To ends up with the same permissions. This does not change
   --  the owner of the file.

   --------------------------
   -- Directory management --
   --------------------------

   function Change_Dir (Dir : not null access File_Record) return Boolean
                        is abstract;
   --  Change the current directory.
   --  This operation might not make sense for some remote file systems if a
   --  new connection is opened for every operation, since the context would
   --  be lost. However, it does make sense when the connection is permanent.

   function Read_Dir
     (Dir            : not null access File_Record;
      Dirs_Only      : Boolean := False;
      Files_Only     : Boolean := False)
      return GNAT.Strings.String_List is abstract;
   --  Read the specified directory and returns a list of filenames
   --  (base names). If Dirs_Only is set, then the files returned are directory
   --  only. Same for Files_Only, concerning regular files.
   --  This does *not* return the two special directories "." and ".."

   function Make_Dir (Dir : not null access File_Record;
                      Recursive : Boolean) return Boolean
                      is abstract;
   --  Create a new directory on remote named Local_Dir_Name.
   --  Return the creation status.

   procedure Remove_Dir
     (Dir       : not null access File_Record;
      Recursive : Boolean;
      Success   : out Boolean)
      is abstract;
   --  Delete a directory. Recursive allow to remove included files or
   --  subdirectories.

   procedure Copy_Dir
     (From    : not null access File_Record;
      Dest    : FS_String;
      Success : out Boolean) is abstract;
   --  From_Local_Name is the name of a directory. All its files are copied
   --  into the directory To_Local_Name. The target directory is created if
   --  needed.

end GNATCOLL.IO;
