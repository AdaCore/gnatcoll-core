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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package body GNATCOLL.Remote.Db is

   Global_Conf : access Remote_Db_Interface'Class := null;

   ---------------------------------
   -- Define_Remote_Configuration --
   ---------------------------------

   procedure Define_Remote_Configuration
     (Config : access Remote_Db_Interface'Class) is
   begin
      Global_Conf := Config;
   end Define_Remote_Configuration;

   -------------------
   -- Is_Configured --
   -------------------

   function Is_Configured (Nickname : String) return Boolean is
   begin
      if Global_Conf = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Global_Conf.Is_Configured (Nickname);
   end Is_Configured;

   ------------
   -- Server --
   ------------

   function Get_Server (Nickname : String) return Server_Access is
   begin
      if Global_Conf = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Global_Conf.Get_Server (Nickname);
   end Get_Server;

   ---------------------
   -- Nb_Mount_Points --
   ---------------------

   function Nb_Mount_Points
     (Nickname : String) return Natural is
   begin
      if Global_Conf = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Global_Conf.Nb_Mount_Points (Nickname);
   end Nb_Mount_Points;

   --------------------------------
   -- Get_Mount_Point_Local_Root --
   --------------------------------

   function Get_Mount_Point_Local_Root
     (Nickname : String;
      Index    : Natural) return FS_String is
   begin
      if Global_Conf = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Global_Conf.Get_Mount_Point_Local_Root (Nickname, Index);
   end Get_Mount_Point_Local_Root;

   -------------------------------
   -- Get_Mount_Point_Host_Root --
   -------------------------------

   function Get_Mount_Point_Host_Root
     (Nickname : String;
      Index    : Natural) return FS_String is
   begin
      if Global_Conf = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Global_Conf.Get_Mount_Point_Host_Root (Nickname, Index);
   end Get_Mount_Point_Host_Root;

end GNATCOLL.Remote.Db;
