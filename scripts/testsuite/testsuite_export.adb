-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007, AdaCore             --
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

with GNAT.Scripts;        use GNAT.Scripts;

package body Testsuite_Export is

   procedure On_Hello   (Data : in out Callback_Data'Class; Command : String);
   procedure C1_Handler (Data : in out Callback_Data'Class; Command : String);

   --------------
   -- On_Hello --
   --------------

   procedure On_Hello (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value (Data, "Hello " & Nth_Arg (Data, 1, "world") & " !");
   end On_Hello;

   ----------------
   -- C1_Handler --
   ----------------

   procedure C1_Handler
      (Data : in out Callback_Data'Class; Command : String)
   is
      --  This could also be kept as a variable somewhere, but fetching it
      --  is relatively cheap
      C1 : constant Class_Type := New_Class (Get_Repository (Data), "C1");

      Inst : Class_Instance := Nth_Arg (Data, 1, C1);
   begin
      if Command = Constructor_Method then
         Set_Data (Inst, C1, Integer'(Nth_Arg (Data, 2)));

      elsif Command = "method" then
         Set_Return_Value
            (Data, "Method applied to class"
             & Integer'Image (Get_Data (Inst, C1)) & " with param "
             & Nth_Arg (Data, 2));
      end if;
   end C1_Handler;

   ------------------------
   -- Register_Functions --
   ------------------------

   procedure Register_Functions (Repo : Scripts_Repository) is
      C1 : Class_Type;
   begin
      Register_Command
        (Repo, "hello", 0, 1,
         Handler => On_Hello'Access);

      C1 := New_Class (Repo, "C1");
      Register_Command
        (Repo, Constructor_Method, 1, 1,
         Class   => C1,
         Handler => C1_Handler'Access);
      Register_Command
        (Repo, "method", 0, 1,
         Class   => C1,
         Handler => C1_Handler'Access);
   end Register_Functions;

end Testsuite_Export;
