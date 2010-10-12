-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                  Copyright (C) 2008-2009, AdaCore                 --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package describes the transport layer used by remote filesystems to
--  communicate with a remote host.
--  Actual implementation is left to the user application, since it requires
--  external knowledge of the environment.
--  Spawning commands might require various application-specific info,
--  such as the protocol to use (ssh, rsh,...) and possibly require the
--  running of commands to setup the environment correctly. These are both
--  specific to your application, and this package does not try to perform
--  this operation as a result.

with GNAT.Expect;
with GNAT.Strings;
with GNATCOLL.VFS_Types; use GNATCOLL.VFS_Types;

package GNATCOLL.Remote is

   --  Server definition

   type Server_Record is interface;
   type Server_Access is access all Server_Record'Class;

   function Nickname
     (Server : Server_Record) return String is abstract;
   function Shell_FS
     (Server : Server_Record) return FS_Type is abstract;

   procedure Execute_Remotely
     (Server              : access Server_Record;
      Args                : GNAT.Strings.String_List;
      Status              : out Boolean;
      Execution_Directory : FS_String := "") is abstract;
   procedure Execute_Remotely
     (Server              : access Server_Record;
      Args                : GNAT.Strings.String_List;
      Result              : out GNAT.Strings.String_Access;
      Status              : out Boolean;
      Execution_Directory : FS_String := "") is abstract;
   --  You must override this subprogram to do the actual spawn of a command on
   --  the specified remote host.

   --  Execution_Directory is the directory in which the command must be run.
   --  The command to execute is passed as the first parameter in Args. Args
   --  must not be freed by these procedure.

   procedure Spawn_Remotely
     (Server              : access Server_Record;
      Descriptor          : out GNAT.Expect.Process_Descriptor_Access;
      Args                : GNAT.Strings.String_List) is abstract;
   --  Spawn a process on the remote machine.
   --  As opposed to Execute_Remotely, this one does not wait until the
   --  process as terminated. Instead, it allows users to interact with the
   --  process by sending commands to it and fetching its output.

end GNATCOLL.Remote;
