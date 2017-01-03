------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

with Ada.Assertions;

package body GNATCOLL.Asserts is

   -------------------------
   -- On_Assertion_Failed --
   -------------------------

   overriding procedure On_Assertion_Failed
      (Self     : Exception_Reporter;
       Msg      : String;
       Details  : String;
       Location : String;
       Entity   : String)
   is
      pragma Unreferenced (Self);
   begin
      raise Ada.Assertions.Assertion_Error with
         Msg & " " & Details & " at " & Location & " in " & Entity;
   end On_Assertion_Failed;

   -------------
   -- Asserts --
   -------------

   package body Asserts is

      -------------------
      -- Assert_Failed --
      -------------------

      procedure Assert_Failed
        (Msg      : String := "";
         Location : String := GNAT.Source_Info.Source_Location;
         Entity   : String := GNAT.Source_Info.Enclosing_Entity) is
      begin
         Report.On_Assertion_Failed
            (Msg      => Msg,
             Details  => "",
             Location => Location,
             Entity   => Entity);
      end Assert_Failed;

      ------------
      -- Equals --
      ------------

      package body Equals is

         ------------------
         -- Assert_Equal --
         ------------------

         procedure Assert_Equal
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity) is
         begin
            if Enabled and then not "=" (Left, Right) then
               Report.On_Assertion_Failed
                  (Details  => Image (Left) & " = " & Image (Right),
                   Msg      => Msg,
                   Location => Location,
                   Entity   => Entity);
            end if;
         end Assert_Equal;

         ----------------------
         -- Assert_Not_Equal --
         ----------------------

         procedure Assert_Not_Equal
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity) is
         begin
            if Enabled and then "=" (Left, Right) then
               Report.On_Assertion_Failed
                  (Details  => Image (Left) & " /= " & Image (Right),
                   Msg      => Msg,
                   Location => Location,
                   Entity   => Entity);
            end if;
         end Assert_Not_Equal;

      end Equals;

      -------------
      -- Compare --
      -------------

      package body Compare is

         ------------------
         -- Assert_Equal --
         ------------------

         procedure Assert_Equal
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity) is
         begin
            if Enabled and then not "=" (Left, Right) then
               Report.On_Assertion_Failed
                  (Details  => Image (Left) & " = " & Image (Right),
                   Msg      => Msg,
                   Location => Location,
                   Entity   => Entity);
            end if;
         end Assert_Equal;

         ----------------------
         -- Assert_Not_Equal --
         ----------------------

         procedure Assert_Not_Equal
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity) is
         begin
            if Enabled and then "=" (Left, Right) then
               Report.On_Assertion_Failed
                  (Details  => Image (Left) & " /= " & Image (Right),
                   Msg      => Msg,
                   Location => Location,
                   Entity   => Entity);
            end if;
         end Assert_Not_Equal;

         -----------------
         -- Assert_Less --
         -----------------

         procedure Assert_Less
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity) is
         begin
            if Enabled and then not "<" (Left, Right) then
               Report.On_Assertion_Failed
                  (Details  => Image (Left) & " < " & Image (Right),
                   Msg      => Msg,
                   Location => Location,
                   Entity   => Entity);
            end if;
         end Assert_Less;

         --------------------------
         -- Assert_Less_Or_Equal --
         --------------------------

         procedure Assert_Less_Or_Equal
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity) is
         begin
            if Enabled
               and then not ("<" (Left, Right) or else "=" (Left, Right))
            then
               Report.On_Assertion_Failed
                  (Details  => Image (Left) & " = " & Image (Right),
                   Msg      => Msg,
                   Location => Location,
                   Entity   => Entity);
            end if;
         end Assert_Less_Or_Equal;

         -----------------------------
         -- Assert_Greater_Or_Equal --
         -----------------------------

         procedure Assert_Greater_Or_Equal
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity) is
         begin
            if Enabled
               and then "<" (Left, Right)
            then
               Report.On_Assertion_Failed
                  (Details  => Image (Left) & " = " & Image (Right),
                   Msg      => Msg,
                   Location => Location,
                   Entity   => Entity);
            end if;
         end Assert_Greater_Or_Equal;

         --------------------
         -- Assert_Greater --
         --------------------

         procedure Assert_Greater
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity) is
         begin
            if Enabled
               and then ("<" (Left, Right) or else "=" (Left, Right))
            then
               Report.On_Assertion_Failed
                  (Details  => Image (Left) & " = " & Image (Right),
                   Msg      => Msg,
                   Location => Location,
                   Entity   => Entity);
            end if;
         end Assert_Greater;

      end Compare;

   end Asserts;

end GNATCOLL.Asserts;
