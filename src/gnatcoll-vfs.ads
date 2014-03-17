------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

--  This package abstracts file operations and names.
--  It is a layer on top of GNATCOLL.Filesystem, which allows you to use the
--  same code to manipulate (copy, rename, delete,...) files, independent of
--  the actual system you are running on (or even if the files happen to be on
--  a remote host).
--  This package provides additional abstraction with regards to file names.
--  Depending on the context, your application will sometime need to use base
--  names (no directory), or full name to reference a file. It is not always
--  clear from the API which type of name is expected, and this package allows
--  you to pass a Virtual_File instead, from which you can extract either the
--  base name or the full name, as needed. This package also abstracts whether
--  file names are case-sensitive or not (in fact, all systems can be
--  considered as case sensitive because file names should be displayed with
--  the exact casing that the user has chosen -- but in some cases the files
--  can be referenced through multiple casing).
--  It also takes care of reference counting, and will therefore free memory as
--  appropriate when it is no longer needed. That makes the type relatively
--  light weight, all the more because most of the information is computed only
--  when needed, and cached in some cases.
--  There is however a cost associated with Virtual_File: they are controlled
--  types, and as such generate a lot of extra code; and they require at least
--  one memory allocation when the file is created to store the name.

pragma Ada_05;

with Ada.Calendar;
with Ada.Containers;
with Ada.Unchecked_Deallocation;
with Ada.Finalization;

with Interfaces.C.Strings;        use Interfaces.C.Strings;

with GNAT.OS_Lib;
with GNAT.Strings;

private with GNATCOLL.IO;
private with GNATCOLL.IO.Native;

package GNATCOLL.VFS is

   ------------------------
   -- Filesystem strings --
   ------------------------

   type Filesystem_String is new String;
   type Filesystem_String_Access is access all Filesystem_String;
   --  A Filesystem_String represents an array of characters as they are
   --  represented on the filesystem, without any encoding consideration.

   function "+" (S : Filesystem_String) return String;
   pragma Inline ("+");
   function "+" (S : String) return Filesystem_String;
   pragma Inline ("+");
   function Equal (S1, S2 : Filesystem_String) return Boolean;
   pragma Inline (Equal);
   procedure Free is new Ada.Unchecked_Deallocation
     (Filesystem_String, Filesystem_String_Access);
   --  Conversion/Comparison/Concatenation functions

   type Cst_Filesystem_String_Access is access constant Filesystem_String;

   ----------------
   -- Exceptions --
   ----------------

   VFS_Directory_Error     : exception;
   VFS_Invalid_File_Error  : exception;
   VFS_Remote_Config_Error : exception;

   ------------------------------
   --  Virtual File definition --
   ------------------------------

   type Virtual_File is tagged private;
   No_File : aliased constant Virtual_File;
   --  Note: a default initialized Virtual_File object has the value No_File

   ---------------
   -- Constants --
   ---------------

   Local_Host : aliased constant String;

   -------------------
   -- Configuration --
   -------------------

   procedure Symbolic_Links_Support (Active : Boolean);
   --  Whether this package should do extra system calls to handle symbolic
   --  links.
   --  This is automatically False on platforms like Windows where this notion
   --  does not exist, but when you know you have no symbolic links manipulated
   --  by your application you can significantly reduce the number of system
   --  calls (which in turns speeds things up). If you set it to False, two
   --  symbolic links that point to the same physical file will be considered
   --  different by the "=" operator. If you set it to True they will be
   --  considered equal.
   --  Changing this is not thread safe. In fact, you should call this before
   --  manipulating any of the Virtual_File, because GNATCOLL.VFS caches the
   --  normalization of file names, and would not redo it for existing files
   --  after you call this function, so the results of "=" in particular might
   --  be unexpected.

   ----------------------------
   --  Creating Virtual_File --
   ----------------------------
   --  The following subprograms are used to create instances of Virtual_File.
   --  On the disk, a filename is typically just a series of bytes, with no
   --  special interpretation in utf8, iso-8859-1 or other pagesets (on most
   --  systems, windows always uses utf8 these days but has other
   --  specificities).
   --  As a result, a filename passed to these Create subprograms will not be
   --  interpreted through an encoding or another, but will just be stored as
   --  is. However, when comes the time to display the file on the disk, the
   --  filename needs to be converted to a known encoding, generally utf8.
   --  See the "Retrieving names" section below.

   function Create
     (Full_Filename : Filesystem_String;
      Host          : String := Local_Host;
      Normalize     : Boolean := False) return Virtual_File;
   --  Return a file, given its full filename.
   --  The latter can be found, for source files, through the functions in
   --  projects-registry.ads.
   --  If Normalize is set, then the VFS is created using the normalized
   --  Full_Filename.

   function Create_From_Dir
     (Dir       : Virtual_File;
      Base_Name : Filesystem_String;
      Normalize : Boolean := False) return Virtual_File;
   --  Creates a file from its directory and base name
   --  If Normalize is set, then Create_From_Dir will make sure that the
   --  path is normalized

   function Create_From_Base
     (Base_Name : Filesystem_String;
      Base_Dir  : Filesystem_String := "";
      Host      : String := Local_Host) return Virtual_File;
   --  Create a file from its base name.
   --  if Base_Name is an absolute path, then the file is created as is
   --  else the file is created relative to Base_Dir or the Current Directory
   --  if provided.

   function Create_From_UTF8
     (Full_Filename : String;
      Host          : String := Local_Host;
      Normalize     : Boolean := False) return Virtual_File;
   --  Creates a file from its display name
   --  If Normalize is set, then the VFS is created using the normalized
   --  Full_Filename.

   function Locate_On_Path
     (Base_Name : Filesystem_String;
      Host      : String := Local_Host) return Virtual_File;
   --  Locate the file from its base name and the PATH environment variable

   ----------------------
   -- Retrieving names --
   ----------------------

   --  As mentioned above, a filename is stored internally as a series of bytes
   --  and not interpreted in anyway for an encoding. However, when you
   --  retrieve the name of a file for display, you will have to convert it to
   --  a known encoding.
   --  There are two sets of functions for retrieving names: Display_* will
   --  return the name converted through the Locale_To_Display function of the
   --  filesystem.
   --  All other functions will return the name as passed to the Create
   --  functions above, and therefore make no guarantee on the encoding of the
   --  file name.

   function Base_Name
     (File      : Virtual_File;
      Suffix    : Filesystem_String := "";
      Normalize : Boolean := False)
      return Filesystem_String;
   --  Return the base name of the file

   function Base_Dir_Name (File : Virtual_File) return Filesystem_String;
   --  Return the base name of the directory or the file

   function Full_Name
     (File          : Virtual_File;
      Normalize     : Boolean := False;
      Resolve_Links : Boolean := False) return Cst_Filesystem_String_Access;
   --  Return the full path to File.
   --  If Normalize is True, the file name is first normalized, note that links
   --  are not resolved there by default, unless you specify Resolve_Links to
   --  True.
   --  The returned value can be used to recreate a Virtual_File instance.
   --  If file names are case insensitive, the normalized name will always
   --  be all lower cases.

   function Full_Name
     (File      : Virtual_File;
      Normalize : Boolean := False) return Filesystem_String;
   --  Same as above, returning a filesystem_string

   function Full_Name_Hash
     (Key : Virtual_File) return Ada.Containers.Hash_Type;
   --  Return a Hash_Type computed from the full name of the given VFS.
   --  Could be used to instantiate an Ada 2005 container that uses a VFS as
   --  key and requires a hash function.
   --  See File_Sets below.

   function File_Extension
     (File      : Virtual_File;
      Normalize : Boolean := False) return Filesystem_String;
   --  Return the extension of the file, or the empty string if there is no
   --  extension. This extension includes the last dot and all the following
   --  characters.
   --  If Normalize is true, the casing is normalized (depending on whether the
   --  platform uses case insensitive file names).

   function Dir_Name (File : Virtual_File) return Filesystem_String;
   --  Return the directory name for File. This includes any available
   --  on the protocol, so that relative files names are properly found.

   function Display_Full_Name
     (File      : Virtual_File;
      Normalize : Boolean := False) return String;
   --  Same as Full_Name

   function Display_Base_Name
     (File   : Virtual_File;
      Suffix : Filesystem_String := "") return String;
   --  Same as Base_Name

   function Display_Dir_Name (File : Virtual_File) return String;
   --  Same as Dir_Name

   function Display_Base_Dir_Name (File : Virtual_File) return String;
   --  Same as Base_Dir_Name

   function Unix_Style_Full_Name
     (File         : Virtual_File;
      Cygwin_Style : Boolean := False;
      Normalize    : Boolean := False;
      Casing       : Boolean := False) return Filesystem_String;
   --  Returns the file path using a unix-style path.
   --  The casing of the filename is not impacted unless Casing is True
   --  (i.e. we do not convert to lower-cases on case-insensitive systems),
   --  because applications should preserve the original casing as much as
   --  possible.

   function Relative_Path
     (File : Virtual_File;
      From : Virtual_File) return Filesystem_String;
   --  Return the path of File relative to From. Return the full_name in case
   --  From and File are not on the same drive.

   function Has_Suffix
     (File : Virtual_File; Suffix : Filesystem_String) return Boolean;
   --  Tell if File has suffix Suffix

   function To_Remote
     (File : Virtual_File; To_Host : String) return Virtual_File;
   --  Convert the file format of File to the convention used on To_Host,
   --  using all available mount points defined for To_Host.

   function To_Local
     (File : Virtual_File) return Virtual_File;
   --  Convert the file format of File to the local filesystem's convention,
   --  potentially using mount points defined between File's host and local
   --  host.

   function To_Arg
     (File : Virtual_File;
      Host : String := Local_Host) return GNAT.Strings.String_Access;
   --  Convert the File to a String Access that can be used as argument for
   --  spawning a process on "Host". The returned value needs to be freeed by
   --  the caller.

   ------------------------
   -- Getting attributes --
   ------------------------

   function Is_Local (File : Virtual_File) return Boolean;
   --  Whether File is local to the host or is a remote file

   function Get_Host (File : Virtual_File) return String;
   --  Retrieve the host of the file, or Local_Host if the file is local to the
   --  host.

   function Is_Regular_File (File : Virtual_File) return Boolean;
   --  Whether File corresponds to an actual file on the disk.
   --  This also works for remote files.

   function Size (File : Virtual_File) return Long_Integer;
   --  The size of the file

   function "=" (File1, File2 : Virtual_File) return Boolean;
   --  Overloading of the standard operator

   function "<" (File1, File2 : Virtual_File) return Boolean;
   --  Compare two files, possibly case insensitively on file systems that
   --  require this.

   function Is_Parent (Parent, Child : Virtual_File) return Boolean;
   --  Compare Parent and Child directory and determines if Parent contains
   --  Child directory

   function Is_Writable (File : Virtual_File) return Boolean;
   --  Return True if File is writable

   function Is_Directory (VF : Virtual_File) return Boolean;
   --  Return True if File is in fact a directory

   function Is_Symbolic_Link (File : Virtual_File) return Boolean;
   --  Return True if File is a symbolic link

   function Is_Absolute_Path (File : Virtual_File) return Boolean;
   --  Return True if File contains an absolute path name, False if it only
   --  contains the base name or a relative name.

   procedure Set_Writable (File : VFS.Virtual_File; Writable : Boolean);
   --  If Writable is True, make File writable, otherwise make File unwritable

   procedure Set_Readable (File : VFS.Virtual_File; Readable : Boolean);
   --  If Readable is True, make File readable, otherwise make File unreadable.
   --  Note that this is not supported on Windows.

   function File_Time_Stamp (File : Virtual_File) return Ada.Calendar.Time;
   --  Return the timestamp for this file. This is GMT time, not local time.
   --  Note: we do not return GNAT.OS_Lib.OS_Time, since the latter cannot be
   --  created by anyone, and is just a private type.
   --  If the file doesn't exist, No_Time is returned.

   procedure Normalize_Path
     (File             : Virtual_File;
      Resolve_Symlinks : Boolean := False);
   --  Resolve '..' and '.' directories in path.
   --  If Resolve_Symlinks is set, then also resolve the symbolic links in
   --  path.

   --------------------
   -- Array of files --
   --------------------

   type File_Array is array (Positive range <>) of aliased Virtual_File;
   type File_Array_Access is access all File_Array;

   procedure Unchecked_Free (Arr : in out File_Array_Access);

   Empty_File_Array : constant File_Array;

   procedure Sort (Files : in out File_Array);
   --  Sort the array of files, in the order given by the full names

   procedure Append (Files : in out File_Array_Access; F : Virtual_File);
   procedure Append (Files : in out File_Array_Access; F : File_Array);
   procedure Prepend (Files : in out File_Array_Access; F : File_Array);
   --  Appends one or more files to Files. Files can be null, in which case a
   --  new File_Array is created.

   procedure Remove (Files : in out File_Array_Access; F : Virtual_File);
   --  Remove F from Files

   function To_Path (Paths : File_Array) return Filesystem_String;
   --  Translates a list of Paths into a path string (e.g. the same format as
   --  $PATH)

   function From_Path (Path : Filesystem_String) return File_Array;
   --  Translate a PATH string into a list of Virtual_File

   function Locate_On_Path
     (Base_Name : Filesystem_String;
      Path      : File_Array) return Virtual_File;
   --  Locate the file from its base name and the furnished list of
   --  directories.

   function Greatest_Common_Path
     (L : GNATCOLL.VFS.File_Array) return Virtual_File;
   --  Return the greatest common path to a list of files or directories
   --  No_File is returned if some files do not have the same root directory.

   function Locate_Regular_File
     (File_Name : Filesystem_String;
      Path      : File_Array) return Virtual_File;
   --  Locate a regular file from its base name and a list of paths

   -------------------------
   --  Manipulating files --
   -------------------------

   procedure Rename
     (File      : Virtual_File;
      Full_Name : Virtual_File;
      Success   : out Boolean);
   --  Rename a file or directory. This does not work for remote files

   procedure Copy
     (File        : Virtual_File;
      Target_Name : Filesystem_String;
      Success     : out Boolean);
   --  Copy a file or directory. This does not work for remote files

   procedure Delete (File : Virtual_File; Success : out Boolean);
   --  Remove file from the disk. This also works for remote files

   function Read_File (File : Virtual_File) return GNAT.Strings.String_Access;
   --  Return the contents of an entire file, encoded with the locale encoding.
   --  If the file cannot be found, return null.
   --  The caller is responsible for freeing the returned memory.
   --  This works transparently for remote files

   --------------------------
   -- Directory operations --
   --------------------------

   Local_Root_Dir : constant Virtual_File;

   function Dir (File : Virtual_File) return Virtual_File;
   --  Return the virtual file corresponding to the directory of the file
   --  If File denotes a directory, then it is returned.
   --  To retrieve the container of File (e.g. get the parent of File, even if
   --  it is a directory), use Get_Parent instead.

   function Get_Current_Dir (Host : String := Local_Host) return Virtual_File;
   --  Current dir on host

   function Get_Tmp_Directory
     (Host : String := Local_Host) return Virtual_File;
   --  Tmp dir on host

   function Get_Home_Directory
     (Host : String := Local_Host) return Virtual_File;
   --  Home dir on host

   function Get_Logical_Drives
     (Host : String := Local_Host) return File_Array_Access;
   --  List of all logical drives on host, or null if none. The list needs to
   --  be freed by the caller.

   procedure Ensure_Directory (Dir : Virtual_File);
   --  Ensures that the file is a directory: add directory separator if
   --  needed.

   function Get_Root (File : Virtual_File) return Virtual_File;
   --  Return root directory of the file

   function Get_Parent (Dir : Virtual_File) return Virtual_File;
   --  Return the parent directory if it exists, else No_File is returned

   function Sub_Dir
     (Dir : Virtual_File; Name : Filesystem_String) return Virtual_File;
   --  Return sub directory Name if it exists, else No_File is returned

   procedure Change_Dir (Dir : Virtual_File);
   --  Changes working directory. Raises Directory_Error if Dir_Name does not
   --  exist or is not a readable directory

   procedure Make_Dir (Dir : Virtual_File; Recursive : Boolean := True);
   --  Create a new directory named Dir_Name. Raises Directory_Error if
   --  Dir_Name cannot be created.
   --  If Recursive, create all intermediary directories needed.

   type Read_Dir_Filter is (All_Files, Dirs_Only, Files_Only);

   function Read_Dir
     (Dir    : Virtual_File;
      Filter : Read_Dir_Filter := All_Files) return File_Array_Access;
   --  Reads all entries from the directory and returns a File_Array containing
   --  those entries, according to filter. The list of files returned
   --  includes directories in systems providing a hierarchical directory
   --  structure, including . (the current directory) and .. (the parent
   --  directory) in systems providing these entries.
   --  The result must be freed by the caller.

   function Read_Dir_Recursive
     (Dir       : Virtual_File;
      Extension : Filesystem_String := "";
      Filter    : Read_Dir_Filter := All_Files) return File_Array_Access;
   --  Reads all entries from the directory, recursively, and returns all
   --  files with the given extension (if specified) that match the filter.
   --  The entries "." and ".." are never returned.
   --  The result must be freed by the caller.

   procedure Remove_Dir
     (Dir       : Virtual_File;
      Recursive : Boolean := False;
      Success   : out Boolean);
   --  Delete the directory Dir. If recursive is True, this also removes all
   --  files or subdirectories contained in it.

   function Read_Files_From_Dirs
     (Dirs : File_Array) return File_Array_Access;
   --  Read all files from the list of directories Dirs

   type Virtual_Dir is private;

   Invalid_Dir : constant Virtual_Dir;

   function Open_Dir (Dir : Virtual_File) return Virtual_Dir;
   --  Opens for reading a file

   procedure Read (VDir : in out Virtual_Dir; File : out Virtual_File);
   --  Returns next file or No_File is no file is left for current directory

   procedure Close (VDir : in out Virtual_Dir);
   --  Closes the Virtual_Dir

   -------------------
   -- Writing files --
   -------------------
   --  Writing is more complex than reading, since generally the whole buffer
   --  to write down is not available immediately, but the user wants to be
   --  able to write characters in a series of calls.
   --  The interface in this package will also support remote files. In this
   --  case, writing the small chunks is done in a temporary file, which is
   --  sent to the remote host only when the file is closed.

   type Writable_File is private;

   Invalid_File : constant Writable_File;
   --  Used when a file couldn't be open

   function Write_File
     (File   : Virtual_File;
      Append : Boolean := False) return Writable_File;
   --  Open File for writing. The returned handler can be used for writting.
   --  You must close it, otherwise the file will not actually be written in
   --  some cases. If Append is True then writting will be done at the end of
   --  the file if the file exists otherwise the file is created.
   --  Return Invalid_File is the file couldn't be open for writing
   --
   --  For safety, the actual writes will occur in a temporary file unless
   --  Append is true, which will be renamed when calling Close. This ensures
   --  that the original file (if there was one) is not destroyed if for some
   --  reason the write fails.

   procedure Write
     (File : in out Writable_File;
      Str  : String);
   procedure Write
     (File : in out Writable_File;
      Str  : chars_ptr);
   --  Write a string to File. The contents of Str are written as-is

   procedure Close (File : in out Writable_File);
   --  Closes File, and write the file to disk.
   --  Use_Error is raised if the file could not be saved.

   ----------------------------------
   -- Some internally used methods --
   ----------------------------------

   function Convert
     (File : Virtual_File; To_Host : String) return Virtual_File;
   function Convert
     (File     : Virtual_File;
      From_Dir : Virtual_File;
      To_Dir   : Virtual_File) return Virtual_File;
   --  Used in mount path conversions. These should be private, but can't
   --  as of RM 3.9.3(10)

private
   --  This type is implemented as a controlled type, to ease the memory
   --  management (so that we can have gtk+ callbacks that take a Virtual
   --  File in argument, without caring who has to free the memory).
   --  Other solutions (using Name_Id to store the strings for instance) do
   --  not work properly, since the functions above cannot modify File
   --  itself, although they do compute some information lazily).

   type Virtual_File is new Ada.Finalization.Controlled with record
      Value : GNATCOLL.IO.File_Access;
   end record;

   pragma Finalize_Storage_Only (Virtual_File);
   procedure Finalize (File : in out Virtual_File);
   procedure Adjust (File : in out Virtual_File);

   type Writable_File is record
      File     : Virtual_File;
      Tmp_File : Virtual_File;
      FD       : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Append   : Boolean;
      Success  : Boolean;
   end record;

   Invalid_File : constant Writable_File :=
     (File     => (Ada.Finalization.Controlled with Value => null),
      Tmp_File => (Ada.Finalization.Controlled with Value => null),
      FD       => GNAT.OS_Lib.Invalid_FD,
      Append   => False,
      Success  => False);

   type Virtual_Dir is record
      File       : Virtual_File;
      Files_List : File_Array_Access;
      Current    : Natural;
   end record;

   No_File : aliased constant Virtual_File :=
     (Ada.Finalization.Controlled with Value => null);

   Local_Host : aliased constant String := "";

   Local_Root_Dir : constant Virtual_File :=
                      (Ada.Finalization.Controlled with
                       Value => GNATCOLL.IO.Native.Local_Root_Dir);

   Empty_File_Array : constant File_Array :=
                        File_Array'(1 .. 0 => No_File);

   Invalid_Dir : constant Virtual_Dir :=
     ((Ada.Finalization.Controlled with Value => null),
      null,
      0);

end GNATCOLL.VFS;
