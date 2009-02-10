-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2006-2009, AdaCore                  --
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

--  This package describes a Unix file system on the local host. See comments
--  in the parent package for more details.

package GNATCOLL.Filesystem.Unix is

   type Unix_Filesystem_Record is new Filesystem_Record with null record;

   overriding function Is_Local (FS : Unix_Filesystem_Record) return Boolean;
   overriding function Dir_Sep (FS : Unix_Filesystem_Record) return Character;
   overriding function To_Unix
     (FS         : Unix_Filesystem_Record;
      Path       : Filesystem_String;
      Use_Cygwin : Boolean := False) return Filesystem_String;
   overriding function From_Unix
     (FS   : Unix_Filesystem_Record;
      Path : Filesystem_String) return Filesystem_String;
   overriding function Is_Absolute_Path
     (FS   : Unix_Filesystem_Record;
      Path : Filesystem_String) return Boolean;
   overriding function Get_Root
     (FS   : Unix_Filesystem_Record;
      Path : Filesystem_String) return Filesystem_String;
   overriding function Device_Name
     (FS   : Unix_Filesystem_Record;
      Path : Filesystem_String) return Filesystem_String;
   overriding function Path
     (FS     : Unix_Filesystem_Record;
      Device : Filesystem_String;
      Dir    : Filesystem_String;
      File   : Filesystem_String) return Filesystem_String;
   overriding function Is_Case_Sensitive
     (FS : Unix_Filesystem_Record) return Boolean;
   overriding function Has_Devices
     (FS : Unix_Filesystem_Record) return Boolean;
   overriding procedure Get_Logical_Drives
     (FS     : Unix_Filesystem_Record;
      Buffer : in out Filesystem_String;
      Len    : out Integer);
end GNATCOLL.Filesystem.Unix;
