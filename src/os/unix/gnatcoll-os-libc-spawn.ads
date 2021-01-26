------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

--  Binding to POSIX C posix_spawn function and related functions.

with GNATCOLL.OS.FS;
with System;

package GNATCOLL.OS.Libc.Spawn is

   package FS renames GNATCOLL.OS.FS;

   type File_Actions is new System.Address;
   --  Pointer to a C posix_spawn_file_actions_t structure

   Null_Actions : constant File_Actions :=
      File_Actions (System.Null_Address);

   type Spawn_Attributes is new System.Address;
   --  Pointer to a C posix_spawnattr_t structure

   Null_Attributes : constant Spawn_Attributes :=
      Spawn_Attributes (System.Null_Address);

   type Process_Id is new Integer;
   --  Process PID

   function Init return File_Actions
   with Import        => True,
        Convention    => C,
        External_Name => "__gnatcoll_posix_spawn_file_actions_init";
   --  Initialize a File_Actions

   procedure Destroy (FA : File_Actions)
   with Import        => True,
        Convention    => C,
        External_Name => "__gnatcoll_posix_spawn_file_actions_destroy";
   --  Free memory hold by a File_Actions structure

   function Add_Dup2
      (Actions : File_Actions;
       Fd      : FS.File_Descriptor;
       Fd2     : FS.File_Descriptor)
       return Integer
   with Import        => True,
        Convention    => C,
        External_Name => "posix_spawn_file_actions_adddup2";
   --  See C posix_spawn_file_actions_adddup2 documentation

   function Add_Close
      (FA : File_Actions;
       Fd : FS.File_Descriptor)
       return Integer
   with Import        => True,
        Convention    => C,
        External_Name => "posix_spawn_file_actions_addclose";
   --  See C posix_spawn_file_actions_addclose documentation

   function Posix_Spawn
      (Pid        : in out Process_Id;
       Path       : C_String;
       Actions    : File_Actions;
       Attributes : Spawn_Attributes;
       Argv       : C_String_Array;
       Env        : C_String_Array)
      return Integer
   with Import        => True,
        Convention    => C,
        External_Name => "posix_spawnp";
   --  See C posix_spawn function

end GNATCOLL.OS.Libc.Spawn;
