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

--  Posix Interface to stat system call

package GNATCOLL.OS.Libc.Stat is

   type Stat_Info is record
      Device  : Uint_64;   --  Id of Device containing the file
      Inode   : Uint_64;   --  Inode number
      Mode    : File_Mode; --  File mode
      Nlink   : Uint_64;   --  Number of hard links
      Uid     : Uint_32;   --  User Id of owner
      Gid     : Uint_32;   --  Group Id of owner
      Rdev    : Uint_64;   --  Device Id (for special files)
      Size    : Sint_64;   --  Total size in bytes
      Blksize : Sint_64;   --  Blocksize for file system I/O
      Blocks  : Sint_64;   --  Number of 512 blocks allocated
      Atime   : Sint_64;   --  Time of last access
      Mtime   : Sint_64;   --  Time of last content modification
      Ctime   : Sint_64;   --  Time of last status change
   end record
   with Convention => C_Pass_By_Copy;

   --  See Posix stat documentation
   function Stat (Path : C_String; Info : in out Stat_Info) return Libc_Status
   with Import        => True,
        Convention    => C,
        External_Name => "__gnatcoll_stat";

   --  See Posix stat documentation
   function Lstat (Path : C_String; Info : in out Stat_Info) return Libc_Status
   with Import        => True,
        Convention    => C,
        External_Name => "__gnatcoll_lstat";

   type Statvfs_Info is record
      Bsize    : Uint_64;  --  File system block size
      Frsize   : Uint_64;  --  Fragment size
      Blocks   : Uint_64;  --  File system size in fragment units (Frsize)
      Bfree    : Uint_64;  --  Free blocks in file system
      Bavail   : Uint_64;  --  Free blocks available to unprivileged users
      Files    : Uint_64;  --  Total file nodes in file system
      Ffree    : Uint_64;  --  Free file nodes in file system
      FAvail   : Uint_64;  --  Free file nodes aviable to unprivileged users
      Flags    : Uint_64;  --  Mount flags (system specific)
      Namemax  : Uint_64;  --  Maximum filename length (note this is the max
                           --  length for each path fragment)
      Pathmax  : Uint_64;  --  Maximum relative path length
                           --  (from current directory). Strictly speaking
                           --  statvfs does not return the max path but this
                           --  value can be useful and thus added to the
                           --  original statvfs structure.
   end record
   with Convention => C_Pass_By_Copy;

   --  See Posix statvfs documentation
   function Statvfs
      (Path : C_String; Info : in out Statvfs_Info) return Libc_Status
   with Import        => True,
        Convention    => C,
        External_Name => "__gnatcoll_statvfs";

   --  See Posix fstatvfs documentation
   function Fstatvfs
      (Fd : FS.File_Descriptor; Info : in out Statvfs_Info) return Libc_Status
   with Import        => True,
        Convention    => C,
        External_Name => "__gnatcoll_fstatvfs";

end GNATCOLL.OS.Libc.Stat;
