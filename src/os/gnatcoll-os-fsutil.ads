------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the impied warranty of MERCHAN- --
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

--  Misc utilities on file content leveraging features from GNATCOLL.OS.FS

with Ada.Strings.UTF_Encoding;
with GNATCOLL.OS.FS;
with GNAT.SHA1;
with GNAT.SHA256;
with Ada.Strings.Unbounded;

package GNATCOLL.OS.FSUtil is

   package UTF8 renames Ada.Strings.UTF_Encoding;
   package FS renames GNATCOLL.OS.FS;
   package UB renames Ada.Strings.Unbounded;

   generic
      type State_Type is private;
      type Result_Type is private;

      Initial_State : State_Type;
      --  Initial state

      with procedure Update (C : in out State_Type; Buffer : String);
      --  Called sequentially on each file chunk to update the state

      with function Result  (C : State_Type) return Result_Type;
      --  Called on the final state to get the result returned by Process

   function Process
      (Path        : UTF8.UTF_8_String;
       Buffer_Size : Positive := FS.Default_Buffer_Size)
      return Result_Type;
   --  Generic function to iterate other a file content and compute some
   --  metrics

   --------------------
   -- Hash functions --
   --------------------

   subtype SHA1_Digest is GNAT.SHA1.Message_Digest;

   function SHA1
      (Path        : UTF8.UTF_8_String;
       Buffer_Size : Positive := FS.Default_Buffer_Size)
      return SHA1_Digest;
   --  Compute the SHA1 of the content of the file located at Path. When
   --  reading the file the function used a finite size Buffer. In most cases
   --  the default buffer size is the one that provide the most performant
   --  way to scan the file.
   --  SHA1_Digest is a fixed sized String. It can be used with both String
   --  and UTF_8_String.

   subtype SHA256_Digest is GNAT.SHA256.Message_Digest;

   function SHA256
      (Path        : UTF8.UTF_8_String;
       Buffer_Size : Positive := FS.Default_Buffer_Size)
      return SHA256_Digest;
   --  Compute the SHA256 of the content of the file located at Path. When
   --  reading the file the function used a finite size Buffer. In most cases
   --  the default buffer size is the one that provide the most performant
   --  way to scan the file.
   --  SHA1_Digest is a fixed sized String. It can be used with both String
   --  and UTF_8_String.

   function Remove_File (Path : UTF8.UTF_8_String) return Boolean;
   --  Remove a file. Return True on success.

   function Copy_File
     (Src                  : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String;
      Preserve_Timestamps  : Boolean := False;
      Preserve_Permissions : Boolean := False) return Boolean;
   --  Copy a file. Return True on success. Preserve arguments
   --  have not effect yet.

   function Copy_Timestamps
     (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String) return Boolean;
   --  Copy timestamps. Return True on success.

   function Copy_Permissions
     (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String) return Boolean;
   --  Copy permissions. Return True on success.

   function Create_Directory (Path : UTF8.UTF_8_String) return Boolean;
   --  Create a directory. Return False on error.

   function Remove_Directory (Path : UTF8.UTF_8_String) return Boolean;
   --  Remove a directory. Return False on error.

   function Create_Symbolic_Link
     (Link_Path : UTF8.UTF_8_String; Target_Path : UTF8.UTF_8_String)
      return Boolean;
   --  Create a symbolic link. Return True on success.

   function Read_Symbolic_Link
     (Link_Path : UTF8.UTF_8_String; Target_Path : out UB.Unbounded_String)
      return Boolean;
   --  Return the symbolic link target path. Return True on success.

   function Copy_Symbolic_Link
     (Src_Path : UTF8.UTF_8_String; Dst_Path : UTF8.UTF_8_String)
     return Boolean;
   --  Copy a symbolic link. Return True on success.

   function Symbolic_Link_Is_Internal
     (Top_Dir_Path : UTF8.UTF_8_String; Link_Path : UTF8.UTF_8_String)
     return Boolean;
   --  Return true if the symbolic link target is internal to the specified
   --  top directory. Several cases make the target external:
   --  - The target is located outside the top directory
   --  - The target is an absolute path
   --  - The target is a relative paths crossing a path outside the top
   --    top directory. For example, the path `../top_dir/my_file`, under
   --    the top directory `top_dir`, is external.

   type Sync_Trees_Mode is (TIMESTAMP, CHECKSUM);
   --  Comparison modes used to decide if a file shall be synchronized.
   --  TIMESTAMP: the timestamp of the file is checked, coupled with the
   --  file length. Faster than CHECKSUM mode, but can make errors.
   --  CHECKSUM: Files content are compared.

   type Sync_Trees_Symlink_Mode is
     (SKIP_SYMLINKS,
     --  Skip symbolic links

     COPY_SYMLINKS,
     --  Copy symbolic links

     COPY_SYMLINKS_TARGET,
     --  Copies symbolic links target instead of the link itself

     COPY_SAFE_SYMLINKS,
     --  Copy only internal symbolic links, and skip external links

     COPY_UNSAFE_SYMLINKS);
   --  Copy internal symbolic links, and copy external symbolic links
   --  targets

   function Sync_Trees
     (Src          : UTF8.UTF_8_String; Dst : String; Mode : Sync_Trees_Mode;
      Symlink_Mode : Sync_Trees_Symlink_Mode := SKIP_SYMLINKS) return Boolean;
   --  Synchronize source and destination directories. Source directory is
   --  copied to the destination one, and entities only present in destination
   --  directory are removed.

end GNATCOLL.OS.FSUtil;
