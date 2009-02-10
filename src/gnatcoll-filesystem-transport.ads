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

package GNATCOLL.Filesystem.Transport is

   type Filesystem_Transport_Record is abstract tagged null record;
   type Filesystem_Transport is access all Filesystem_Transport_Record'Class;

   procedure Execute_Remotely
     (Transport           : access Filesystem_Transport_Record;
      Host                : String;
      Args                : GNAT.Strings.String_List;
      Status              : out Boolean;
      Execution_Directory : Filesystem_String := "") is abstract;
   procedure Execute_Remotely
     (Transport           : access Filesystem_Transport_Record;
      Host                : String;
      Args                : GNAT.Strings.String_List;
      Result              : out GNAT.Strings.String_Access;
      Status              : out Boolean;
      Execution_Directory : Filesystem_String := "") is abstract;
   --  You must override this subprogram to do the actual spawn of a command on
   --  the specified remote host.

   --  Execution_Directory is the directory in which the command must be run.
   --  The command to execute is passed as the first parameter in Args. Args
   --  must not be freed by these procedure.

   procedure Spawn_Remotely
     (Transport           : access Filesystem_Transport_Record;
      Descriptor          : out GNAT.Expect.Process_Descriptor_Access;
      Host                : String;
      Args                : GNAT.Strings.String_List) is abstract;
   --  Spawn a process on the remote machine.
   --  As opposed to Execute_Remotely, this one does not wait until the
   --  process as terminated. Instead, it allows users to interact with the
   --  process by sending commands to it and fetching its output.

end GNATCOLL.Filesystem.Transport;
