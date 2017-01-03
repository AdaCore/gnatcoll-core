------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;
with GNATCOLL.Scripts.Shell;  use GNATCOLL.Scripts.Shell;

package body Common is

   procedure On_Hello (Data : in out Callback_Data'Class; Command : String);

   procedure On_Hello (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value (Data, "Hello " & Nth_Arg (Data, 1, "world") & " !");
   end On_Hello;

   ------------------------------------
   -- Register_Scripts_And_Functions --
   ------------------------------------

   function Register_Scripts_And_Functions return Scripts_Repository is
      Repo : Scripts_Repository;
   begin
      --  Register all scripting languages. In practice, you only need to
      --  register those you intend to support

      Repo := new Scripts_Repository_Record;
      Register_Shell_Scripting  (Repo);
      Register_Python_Scripting (Repo, "Hello");
      Register_Standard_Classes (Repo, "Console");

      --  Now register our custom functions. Note that we do not need to
      --  register them once for every support language, once is enough, they
      --  are automatically exported to all registered languages.

      --  Available as "Hello.hello("world")" in python,
      --  and "hello world" in shell script
      Register_Command
        (Repo, "hello", 0, 1,
         Handler => On_Hello'Unrestricted_Access);

      return Repo;
   end Register_Scripts_And_Functions;

end Common;
