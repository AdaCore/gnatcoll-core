-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2006-2008, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package is used to describe a complete filesystem and how to
--  manipulate files and directories.
--  A default implementation valid for both windows and unix local filesystems
--  is provided, but this type still needs to be overridden to provide
--  system-specific implementation in some cases. It is also possible to
--  further derive this type to implement remote file systems, ie access to
--  files on remote hosts.

with Ada.Calendar;
with GNAT.Strings;

package GNATCOLL.Filesystem is

   type Filesystem_Record is abstract tagged private;
   type Filesystem_Access is access all Filesystem_Record'Class;

   function Get_Local_Filesystem return Filesystem_Access;
   --  Return an instance of the local filesystem running on the current host.
   --  This notion is slightly ambiguous when one involves NFS mounts (for
   --  instance a Unix machine might be mounting a windows filesystem), but
   --  the value returned is the one for native filesystems on that machine (in
   --  the example above that would be an instance of Unix_Filesystem).
   --  The returned value must not be freed by the caller.

   procedure Free (FS : in out Filesystem_Record) is null;
   procedure Free (FS : in out Filesystem_Access);
   --  Free the memory allocated for FS

   function Dir_Sep (FS : Filesystem_Record) return Character is abstract;
   --  Return the filesystem's directory separator

   function To_Unix
     (FS         : Filesystem_Record;
      Path       : String;
      Use_Cygwin : Boolean := False) return String is abstract;
   --  Translate a Path to unix style

   function From_Unix
     (FS   : Filesystem_Record;
      Path : String) return String is abstract;
   --  Translate a Path from unix style

   function Is_Subtree
     (FS        : Filesystem_Record;
      Directory : String;
      Full_Path : String) return Boolean;
   --  Tell if Full_Path is in the subtree of Directory
   --  By default, it compares the two strings, and return true if the first
   --  part of Full_Path is equal to directory, taking into account the
   --  case sensitivity.

   function Is_Absolute_Path
     (FS   : Filesystem_Record;
      Path : String) return Boolean is abstract;
   --  Tell wether the path is absolute

   function File_Extension
     (FS   : Filesystem_Record;
      Path : String) return String;
   --  Return the file extension
   --  By default, return the characters from the last dot (included) to the
   --  end of the Path.

   function Concat
     (FS   : Filesystem_Record;
      Root : String;
      Sub  : String) return String;
   --  Concatenate a root directory and a subdirectory
   --  by default, equivalent to 'Root & Sub', after ensuring that Root does
   --  end with a directory separator.

   function Base_Name
     (FS     : Filesystem_Record;
      Path   : String;
      Suffix : String := "") return String;
   --  Return the base file name

   function Base_Dir_Name
     (FS   : Filesystem_Record;
      Path : String) return String;
   --  Return the directory base name

   function Dir_Name
     (FS   : Filesystem_Record;
      Path : String) return String;
   --  Return the directory path

   function Locale_To_Display
     (FS : Filesystem_Record; Name : String) return String;
   --  Convert a file name (ie an unspecified set of bytes) to a specific
   --  encoding (application specific). In general, this encoding will be
   --  UTF-8, so that you can display the name in a graphical interface, but
   --  you could chose to override this function and use another encoding
   --  instead.
   --  The default is to call the function registered through
   --  Set_Locale_To_Display_Encoder (or return Name as is if not set). This is
   --  suitable only if files on that FS are encoded in UTF-8 by default (true
   --  only on windows platforms), but is suitable even on other platforms if
   --  you do not support accented file names for instance.
   --  This function is only called implicitly from GNATCOLL.VFS.Display_*, but
   --  if you are using the GNATCOLL.Filesystem API directly, you should
   --  convert file names appropriately before you display them on the screen.

   type Encoder_Function is access function (Name : String) return String;

   procedure Set_Locale_To_Display_Encoder
     (FS      : in out Filesystem_Record;
      Encoder : Encoder_Function);
   --  Set the function used to convert locale file names (a series of bytes)
   --  into a properly encoded string (the specific encoding to use depends on
   --  your application, but will generally be utf-8).
   --  Filesystems use both a primitive operation (Locale_To_Display) and
   --  a callback for this operation to give the most flexibility to the
   --  application (having only the primitive op would mean overriding all the
   --  predefined types of filesystems in the application's code -- having only
   --  the callback means it is somewhat more work to convert file names since
   --  the application would have to check whether the encoder has been set).

   function Get_Root
     (FS   : Filesystem_Record;
      Path : String) return String is abstract;
   --  Return the root directory of the path

   function Get_Tmp_Directory (FS : Filesystem_Record) return String;
   --  Return the name of a directory that can be used to store temporary
   --  directory on the filesystem. That directory always ends with a directory
   --  separator (when appropriate for the file system)

   function Get_Parent
     (FS   : Filesystem_Record;
      Path : String) return String;
   --  Return the parent directory of the path

   function Ensure_Directory
     (FS   : Filesystem_Record;
      Path : String) return String;
   --  Return a directory path from furnished path.
   --  On Windows, for a path C:\path\to, this will return C:\path\to\
   --  On VMS, for a path disk:[path]to.dir, this will return disk:[path.to]

   function Device_Name
     (FS   : Filesystem_Record;
      Path : String) return String is abstract;
   --  Return the device of the path (if applicable). Empty string otherwise

   function Normalize
     (FS   : Filesystem_Record;
      Path : String) return String;
   --  Replace every ./ or ../ items of the path

   function Path
     (FS     : Filesystem_Record;
      Device : String;
      Dir    : String;
      File   : String) return String is abstract;
   --  Return a path composed of Device, Dir, and File

   function Is_Case_Sensitive
     (FS : Filesystem_Record) return Boolean is abstract;
   --  Tell if the Filesystem_Record is case sensitive

   function Has_Devices
     (FS : Filesystem_Record) return Boolean is abstract;
   --  Tell if the Filesystem handles devices (hard disk letters for windows)

   function Multi_Unit_Index_Char
     (FS : Filesystem_Record) return Character;
   --  The character used by GNAT when creating ALI files for multi-unit files
   --  on the given filesystem (this is generally '~' expect on VMS where it is
   --  set to '$').

   -------------------------
   -- Operations on files --
   -------------------------

   function Home_Dir (FS   : Filesystem_Record) return String;
   --  Return the home directory on the specified host.
   --  If home dir cannot be determined, return root directory

   function Is_Regular_File
     (FS              : Filesystem_Record;
      Local_Full_Name : String) return Boolean;
   --  Return True if Local_Full_Name exists on the remote host

   function Is_Symbolic_Link
     (FS              : Filesystem_Record;
      Local_Full_Name : String) return Boolean;
   --  Whether the file is a symbolic link

   function Read_File
     (FS              : Filesystem_Record;
      Local_Full_Name : String) return GNAT.Strings.String_Access;
   --  Return the contents of an entire file.
   --  If the file cannot be found, return null.
   --  The caller is responsible for freeing the returned memory.
   --  No special encoding/decoding for charsets is done on the file.

   function Delete
     (FS              : Filesystem_Record;
      Local_Full_Name : String) return Boolean;
   --  Sends host a delete command for file

   function Rename
     (FS              : Filesystem_Record;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean;
   --  Rename From_Local_Name on the host to To_Local_Name on the same host.
   --  Return False if the renaming could not be performed.

   function Copy
     (FS              : Filesystem_Record;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean;
   --  Copy a file into another one.
   --  To_Local_Name can be the name of the directory in which to copy the
   --  file, or the name of a file to be created.

   function Copy_Dir
     (FS              : Filesystem_Record;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean;
   --  From_Local_Name is the name of a directory. All its files are copied
   --  into the directory To_Local_Name. The target directory is created if
   --  needed.

   function Is_Writable
     (FS              : Filesystem_Record;
      Local_Full_Name : String) return Boolean;
   --  Return True if File is writable.
   --  Some protocols are read-only (HTTP), and will always return False.

   function Is_Directory
     (FS              : Filesystem_Record;
      Local_Full_Name : String) return Boolean;
   --  Return True if File is in fact a directory

   function File_Time_Stamp
     (FS              : Filesystem_Record;
      Local_Full_Name : String) return Ada.Calendar.Time;
   --  Return the timestamp for this file.
   --  If the Connection doesn't support this operation, or the file
   --  doesn't exists, it should return a date of No_Time, so as to force, when
   --  possible, a read operation from the caller.

   procedure Write
     (FS              : Filesystem_Record;
      Local_Full_Name : String;
      Temporary_File  : String;
      Append          : Boolean := False);
   --  Overwrite the contents of Local_Full_Name with the contents of the
   --  Temporary_File.
   --  Raise Use_Error if the file could not be written.

   procedure Set_Writable
     (FS              : Filesystem_Record;
      Local_Full_Name : String;
      Writable        : Boolean);
   --  If Writable is True, make the file writable, otherwise make the file
   --  unwritable.

   procedure Set_Readable
     (FS              : Filesystem_Record;
      Local_Full_Name : String;
      Readable        : Boolean);
   --  If Readable is True, make the file readable, otherwise make the file
   --  unreadable.

   --------------------------
   -- Directory management --
   --------------------------

   procedure Get_Logical_Drives
     (FS     : Filesystem_Record;
      Buffer : in out String;
      Len    : out Integer) is abstract;
   --  Store in Buffer (Buffer'First .. Buffer'First + Len) a ASCII.NUL
   --  separated string containing the names of the drives, e.g
   --  "a:\" & NUL & "c:\", or a null string if not relevant on the target.

   function Make_Dir
     (FS             : Filesystem_Record;
      Local_Dir_Name : String) return Boolean;
   --  Create a new directory on remote named Local_Dir_Name.
   --  Return the creation status.

   function Change_Dir
     (FS             : Filesystem_Record;
      Local_Dir_Name : String) return Boolean;
   --  Change the current directory.
   --  This operation might not make sense for some remote file systems if a
   --  new connection is opened for every operation, since the context would
   --  be lost. However, it does make sense when the connection is permanent.

   function Remove_Dir
     (FS             : Filesystem_Record;
      Local_Dir_Name : String;
      Recursive      : Boolean) return Boolean;
   --  Delete an empty directory on remote named Local_Dir_Name.
   --  Return the deletion status.

   function Read_Dir
     (FS             : Filesystem_Record;
      Local_Dir_Name : String;
      Dirs_Only      : Boolean := False;
      Files_Only     : Boolean := False)
      return GNAT.Strings.String_List;
   --  Read the specified directory and returns a list of filenames
   --  (base names). If Dirs_Only is set, then the files returned are directory
   --  only. Same for Files_Only, concerning regular files.
   --  This does not return the two special directories "." and ".."

private
   type Filesystem_Record is abstract tagged record
      Locale_To_Display_Encoder : Encoder_Function;
   end record;

end GNATCOLL.Filesystem;
