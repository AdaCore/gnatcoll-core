------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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

with GNATCOLL.VFS_Types; use GNATCOLL.VFS_Types;

private package GNATCOLL.Path is

   Invalid_Filesystem : exception;
   --  Raised when calling any of the below methods with FS_Unknown

   -------------------
   -- FS Properties --
   -------------------

   function Local_FS return FS_Type;
   pragma Inline (Local_FS);
   --  Get the local FS type

   function Is_Case_Sensitive (FS : FS_Type) return Boolean;
   --  Tell if FS is case sensitive

   function Has_Devices (FS : FS_Type) return Boolean;
   --  Tell if the Filesystem handles devices (hard disk letters for windows)

   function Multi_Unit_Index_Char (FS : FS_Type) return Character;
   --  The character used by GNAT when creating ALI files for multi-unit files
   --  on the given filesystem (this is generally '~' expect on VMS where it is
   --  set to '$').

   function Exe_Extension (FS : FS_Type) return FS_String;
   --  .exe on Windows, nothing on Unix

   ------------------
   -- Path queries --
   ------------------

   function Get_Root
     (FS   : FS_Type;
      Path : FS_String) return FS_String;
   --  Return the root directory of the path

   function Is_Absolute_Path
     (FS   : FS_Type;
      Path : FS_String) return Boolean;
   --  Tell wether the path is absolute

   ------------------------
   -- Path manipulations --
   ------------------------

   function Path
     (FS     : FS_Type;
      Device : FS_String;
      Dir    : FS_String;
      File   : FS_String) return FS_String;
   --  Return a path composed of Device, Dir, and File

   function Equal
     (FS           : FS_Type;
      Path1, Path2 : FS_String) return Boolean;
   --  Tell if Path1 and Path2 are equivalent

   function To_Unix
     (FS          : FS_Type;
      Path        : FS_String;
      Cygwin_Path : Boolean := False) return FS_String;
   --  Translate a Path to unix style

   function From_Unix
     (FS   : FS_Type;
      Path : FS_String) return FS_String;
   --  Translate a Path from unix style

   function File_Extension
     (FS   : FS_Type;
      Path : FS_String) return FS_String;
   --  Return the file extension, including the last '.'

   function Base_Name
     (FS     : FS_Type;
      Path   : FS_String;
      Suffix : FS_String := "") return FS_String;
   --  Return the base file name

   function Base_Dir_Name
     (FS   : FS_Type;
      Path : FS_String) return FS_String;
   --  Return the directory base name. Root directories will be returned
   --  as-is ("/", "C:\", "\\machine\service\")

   function Get_Parent
     (FS : FS_Type;
      Path : FS_String) return FS_String;
   --  Return the parent directory of Path. This differs from Dir_Name in that
   --  calling Get_Parent on a directory will return the directory's parent.

   function Dir_Name
     (FS   : FS_Type;
      Path : FS_String) return FS_String;
   --  Return the directory path. Calling Dir_Name on a directory will return
   --  the directory itself.

   function Is_Dir_Name
     (FS   : FS_Type;
      Path : FS_String) return Boolean;
   --  Return true if Path denotes a directory path in FS (e.g. ends with a
   --  directory separator).

   function Ensure_Directory
     (FS   : FS_Type;
      Path : FS_String) return FS_String;
   --  Return a directory path from furnished path.
   --  On Windows, for a path C:\path\to, this will return C:\path\to\
   --  On VMS, for a path disk:[path]to.dir, this will return disk:[path.to]

   function Device_Name
     (FS   : FS_Type;
      Path : FS_String) return FS_String;
   --  Return the device of the path (if applicable). Empty string otherwise

   function Normalize
     (FS   : FS_Type;
      Path : FS_String) return FS_String;
   --  Replace every ./ or ../ items of the path

   function Relative_Path
     (FS   : FS_Type;
      Ref  : FS_String;
      Path : FS_String) return FS_String;
   --  Return the path of Path relative to Ref

end GNATCOLL.Path;
