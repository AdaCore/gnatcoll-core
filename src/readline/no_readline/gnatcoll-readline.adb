------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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

with Ada.Text_IO;

package body GNATCOLL.Readline is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Appname      : String := "";
       History_File : String := "";
       Completer    : Completer_Function := null)
   is
      pragma Unreferenced (Appname, History_File, Completer);
   begin
      null;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (History_File : String := "") is
      pragma Unreferenced (History_File);
   begin
      null;
   end Finalize;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Prompt : String := "") return String is
   begin
      Ada.Text_IO.Put (Prompt);
      return Ada.Text_IO.Get_Line;
   end Get_Line;

   ------------------------
   -- Completion_Matches --
   ------------------------

   function Completion_Matches
      (Text      : String;
       Generator : Completion_Entry_Func)
       return Possible_Completions
   is
      pragma Unreferenced (Text, Generator);
   begin
      return null;
   end Completion_Matches;
end GNATCOLL.Readline;
