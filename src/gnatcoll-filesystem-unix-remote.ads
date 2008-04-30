-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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

--  This package describes a unix filesystem on a remote host. All file
--  manipulation are done by spawning shell commands.

with GNATCOLL.Filesystem.Transport;

package GNATCOLL.Filesystem.Unix.Remote is

   type Remote_Unix_Filesystem_Record
     is new Unix_Filesystem_Record with private;

   procedure Setup
     (FS        : in out Remote_Unix_Filesystem_Record;
      Host      : String;
      Transport : not null access
        GNATCOLL.Filesystem.Transport.Filesystem_Transport_Record'Class);
   --  Change the name of the remote host on which the filesystem is running,
   --  and the way to communicate with that system.
   --  This function _must_ be called before any use of the filesystem

   function Get_Host (FS : Remote_Unix_Filesystem_Record) return String;
   --  Return the host on which the filesystem is running

   overriding function Home_Dir
     (FS   : Remote_Unix_Filesystem_Record) return String;
   overriding function Is_Regular_File
     (FS              : Remote_Unix_Filesystem_Record;
      Local_Full_Name : String) return Boolean;
   overriding function Read_File
     (FS              : Remote_Unix_Filesystem_Record;
      Local_Full_Name : String) return GNAT.Strings.String_Access;
   overriding function Delete
     (FS              : Remote_Unix_Filesystem_Record;
      Local_Full_Name : String) return Boolean;
   overriding function Is_Writable
     (FS              : Remote_Unix_Filesystem_Record;
      Local_Full_Name : String) return Boolean;
   overriding function Is_Symbolic_Link
     (FS              : Remote_Unix_Filesystem_Record;
      Local_Full_Name : String) return Boolean;
   overriding function Is_Directory
     (FS              : Remote_Unix_Filesystem_Record;
      Local_Full_Name : String) return Boolean;
   overriding function File_Time_Stamp
     (FS              : Remote_Unix_Filesystem_Record;
      Local_Full_Name : String)
      return Ada.Calendar.Time;
   overriding procedure Write
     (FS              : Remote_Unix_Filesystem_Record;
      Local_Full_Name : String;
      Temporary_File  : String;
      Append          : Boolean := False);
   overriding procedure Set_Writable
     (FS              : Remote_Unix_Filesystem_Record;
      Local_Full_Name : String;
      Writable        : Boolean);
   overriding procedure Set_Readable
     (FS              : Remote_Unix_Filesystem_Record;
      Local_Full_Name : String;
      Readable        : Boolean);
   overriding function Make_Dir
     (FS             : Remote_Unix_Filesystem_Record;
      Local_Dir_Name : String) return Boolean;
   overriding function Remove_Dir
     (FS             : Remote_Unix_Filesystem_Record;
      Local_Dir_Name : String;
      Recursive      : Boolean) return Boolean;
   overriding function Read_Dir
     (FS             : Remote_Unix_Filesystem_Record;
      Local_Dir_Name : String;
      Dirs_Only      : Boolean := False;
      Files_Only     : Boolean := False) return GNAT.Strings.String_List;
   overriding function Rename
     (FS              : Remote_Unix_Filesystem_Record;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean;
   overriding function Copy
     (FS              : Remote_Unix_Filesystem_Record;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean;
   overriding function Copy_Dir
     (FS              : Remote_Unix_Filesystem_Record;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean;
   overriding function Change_Dir
     (FS             : Remote_Unix_Filesystem_Record;
      Local_Dir_Name : String) return Boolean;
   overriding procedure Free (FS : in out Remote_Unix_Filesystem_Record);
   --  See inherited documentation in parent package

private
   type Remote_Unix_Filesystem_Record is new Unix_Filesystem_Record with
      record
         Host      : GNAT.Strings.String_Access;
         Transport : GNATCOLL.Filesystem.Transport.Filesystem_Transport;
      end record;

end GNATCOLL.Filesystem.Unix.Remote;
