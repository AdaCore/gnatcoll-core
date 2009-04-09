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

--  This package is used to keep a global configuration of servers.
--  There are two almost identical parts: one defining static methods that
--  can be used by other GNATCOLL packages (such as GNATCOLL.IO.Remote),
--  and the second part that defines the actual configuration class, that
--  needs to be implemented and registered by the user code.

package GNATCOLL.Remote.Db is

   Invalid_Remote_Configuration : exception;

   ---------------------------------------------------
   -- The remote configuration interface definition --
   ---------------------------------------------------

   type Remote_Db_Interface is interface;

   function Is_Configured
     (Config   : Remote_Db_Interface;
      Nickname : String) return Boolean is abstract;
   --  Tell if a server with this name exists in the global configuration

   function Get_Server
     (Config   : Remote_Db_Interface;
      Nickname : String) return Server_Access is abstract;
   --  Get the server from its nickname.

   function Nb_Mount_Points
     (Config   : Remote_Db_Interface;
      Nickname : String) return Natural is abstract;
   --  Get the number of mount points defined for the server

   function Get_Mount_Point_Local_Root
     (Config   : Remote_Db_Interface;
      Nickname : String;
      Index    : Natural) return FS_String is abstract;
   --  Get the local mount point

   function Get_Mount_Point_Host_Root
     (Config   : Remote_Db_Interface;
      Nickname : String;
      Index    : Natural) return FS_String is abstract;
   --  Get the remote point.

   ---------------------------------------------------------
   -- Needs to be called before any of the static methods --
   ---------------------------------------------------------

   procedure Define_Remote_Configuration
     (Config : access Remote_Db_Interface'Class);
   --  Defines the remote configuration that will be used for all remote access
   --  performed by GNATCOLL.

   -------------------------------------------------
   --  Static methods used by GNATCOLL internally --
   -------------------------------------------------

   function Is_Configured (Nickname : String) return Boolean;
   --  Tell if a server with this name exists in the global configuration
   --  Raise Invalid_Remote_Config if no global configuration has been defined.

   function Get_Server (Nickname : String) return Server_Access;
   --  Get the server from its nickname.
   --  Raise Invalid_Remote_Config if no global configuration has been defined.

   function Nb_Mount_Points
     (Nickname : String) return Natural;
   --  Get the number of mount points defined for the server
   --  Raise Invalid_Remote_Config if no global configuration has been defined.

   function Get_Mount_Point_Local_Root
     (Nickname : String;
      Index    : Natural) return FS_String;
   --  Get the local mount point
   --  Raise Invalid_Remote_Config if no global configuration has been defined.

   function Get_Mount_Point_Host_Root
     (Nickname : String;
      Index    : Natural) return FS_String;
   --  Get the remote point.
   --  Raise Invalid_Remote_Config if no global configuration has been defined.

end GNATCOLL.Remote.Db;
