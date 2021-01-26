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
with GNATCOLL.OS.FS;
with GNATCOLL.OS.Libc_Constants;
with Interfaces.C; use Interfaces.C;

package GNATCOLL.OS.Libc is

   package FS renames GNATCOLL.OS.FS;
   package Constants renames GNATCOLL.OS.Libc_Constants;

   type Uint_64 is mod 2 ** Long_Long_Integer'Size;
   type Uint_32 is mod 2 ** Integer'Size;
   subtype Sint_64 is Long_Long_Integer;
   subtype Sint_32 is Integer;

   type Uint is mod 2 ** Standard'Address_Size;

   --  Status returned by most libc functions.
   subtype Libc_Status is Sint_32 range -1 .. 0;
   Success : constant Libc_Status := 0;
   Error   : constant Libc_Status := -1;

   --  File modes
   type File_Mode is new Uint_32;

   S_IRWXU  : constant File_Mode := 8#0700#;
   S_IRUSR  : constant File_Mode := 8#0400#;
   S_IWUSR  : constant File_Mode := 8#0200#;
   S_IXUSR  : constant File_Mode := 8#0100#;

   S_IRWXG  : constant File_Mode := 8#0070#;
   S_IRGRP  : constant File_Mode := 8#0040#;
   S_IWGRP  : constant File_Mode := 8#0020#;
   S_IXGRP  : constant File_Mode := 8#0010#;

   S_IRWXO  : constant File_Mode := 8#0007#;
   S_IROTH  : constant File_Mode := 8#0004#;
   S_IWOTH  : constant File_Mode := 8#0002#;
   S_IXOTH  : constant File_Mode := 8#0001#;

   S_IFDIR  : constant File_Mode := 8#040000#;
   S_IFCHR  : constant File_Mode := 8#020000#;
   S_IFBLK  : constant File_Mode := 8#060000#;
   S_IFREG  : constant File_Mode := 8#100000#;
   S_IFIFO  : constant File_Mode := 8#010000#;
   S_IFLNK  : constant File_Mode := 8#120000#;
   S_IFSOCK : constant File_Mode := 8#140000#;
   S_IFMT   : constant File_Mode := 8#170000#;

   --  File open modes
   type Open_Mode is new Uint_32;

   O_RDONLY   : constant Open_Mode := Constants.O_RDONLY;
   O_WRONLY   : constant Open_Mode := Constants.O_WRONLY;
   O_RDWR     : constant Open_Mode := Constants.O_RDWR;
   O_CREAT    : constant Open_Mode := Constants.O_CREAT;
   O_EXCL     : constant Open_Mode := Constants.O_EXCL;
   O_NOCTTY   : constant Open_Mode := Constants.O_NOCTTY;
   O_TRUNC    : constant Open_Mode := Constants.O_TRUNC;
   O_APPEND   : constant Open_Mode := Constants.O_APPEND;
   O_NONBLOCK : constant Open_Mode := Constants.O_NONBLOCK;
   O_CLOEXEC  : constant Open_Mode := Constants.O_CLOEXEC;

   --  Fnctl commands
   subtype Fnctl_Cmd is Integer;
   F_GETFD : constant Fnctl_Cmd := 1;
   F_SETFD : constant Fnctl_Cmd := 2;
   FD_CLOEXEC : constant Uint := 1;

   --  Define a few priority used to map to GNATCOLL.OS.Process priority class
   subtype Priority is Integer range -20 .. 20;
   Idle_Priority         : constant Priority := 19;
   Below_Normal_Priority : constant Priority := 10;
   Normal_Priority       : constant Priority := 0;
   Above_Normal_Priority : constant Priority := -10;
   High_Priority         : constant Priority := -20;

   --  The following value does not map to a system priority value. It
   --  is used when implementing the semantic of "inherit" priority
   --  (i.e: in which case setpriority should not be called)
   Inherit_Priority      : constant Priority := 20;

   --  Elements on which a priority can apply
   subtype Priority_Target is Integer range 0 .. 2;
   Process_Priority       : constant Priority_Target := 0;
   Process_Group_Priority : constant Priority_Target := 1;
   User_Priority          : constant Priority_Target := 2;

   --  Used in pipe calls
   type Pipe_Type is record
      Input  : OS.FS.File_Descriptor;
      Output : OS.FS.File_Descriptor;
   end record;

   function Isatty
     (Fd : FS.File_Descriptor)
     return Integer
   with Import        => True,
        Convention    => C,
        External_Name => "isatty";
   --  See Posix isatty

   function Open
      (Path : C_String;
       Mode : Open_Mode;
       Perm : File_Mode)
      return FS.File_Descriptor
   with Import        => True,
        Convention    => C,
        External_Name => "__gnatcoll_open";
   --  See Posix open

   function Fcntl
      (Fd  : FS.File_Descriptor;
       Cmd : Fnctl_Cmd;
       Arg : Integer)
      return Integer
   with Import        => True,
        Convention    => C,
        External_Name => "fcntl";
   --  See Posix fcntl

   function Pipe (Fds : not null access Pipe_Type) return Libc_Status
   with Import        => True,
        Convention    => C,
        External_Name => "__gnatcoll_pipe";
   --  See Posix pipe
   --  Note that on linux this function sets the O_CLOEXEC flag to the returned
   --  pipes.

   --  See Posix chdir documentation
   function Chdir (Path : C_String) return Libc_Status
   with Import        => True,
        Convention    => C,
        External_Name => "chdir";

   --  See Posix getcwd documentation
   function Getcwd (Buf : C_String; Size : size_t) return System.Address
   with Import        => True,
        Convention    => C,
        External_Name => "getcwd";

   --  See Posix setpriority documentation
   function Setpriority (Which : Priority_Target;
                         Who   : Integer;
                         Prio  : Priority)
                        return Libc_Status
   with Import        => True,
        Convention    => C,
        External_Name => "setpriority";

   function Waitpid (Pid     : Integer;
                     Status  : in out Uint_32;
                     Options : Integer)
                    return Integer
   with Import        => True,
        Convention    => C,
        External_Name => "waitpid";
end GNATCOLL.OS.Libc;
