------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
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
