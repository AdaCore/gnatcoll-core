-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Copyright (C) 2017, AdaCore                                         --
--                                                                           --
--  GNADE is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with GNAT;  see file COPYING.  If not, write --
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception,  if other files  instantiate  generics from this --
--  unit, or you link  this unit with other files  to produce an executable, --
--  this  unit  does not  by itself cause  the resulting  executable  to  be --
--  covered  by the  GNU  General  Public  License.  This exception does not --
--  however invalidate  any other reasons why  the executable file  might be --
--  covered by the  GNU Public License.                                      --
--                                                                           --
--  GNADE is implemented to work with GNAT, the GNU Ada compiler.            --
-------------------------------------------------------------------------------

--  This file is to build GNATCOLL without postgres support

limited with Interfaces.C;

package GNATCOLL.SQL.Postgres.Gnade is

   type Database is tagged null record;

   function Socket (DB : Database'Class) return Interfaces.C.int is (0);

   procedure Notifies
     (DB      : Database'Class;
      Message : out Notification;
      Done    : out Boolean) is null;

   function Consume_Input (DB : Database'Class) return Boolean is (False);

   function Error (DB : Database'Class) return String is ("");

   function Is_Non_Blocking (DB : Database'Class) return Boolean is (False);

end GNATCOLL.SQL.Postgres.Gnade;
