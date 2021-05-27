------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with GNATCOLL.WString_Builders;
with GNATCOLL.WString_List_Builders;
with GNATCOLL.OS.Win32;
with Ada.Strings.UTF_Encoding;

package GNATCOLL.OS.Process_Types is

   package UTF8 renames Ada.Strings.UTF_Encoding;
   package OS renames GNATCOLL.OS;

   --  Win32 process handle
   type Process_Handle is new OS.Win32.HANDLE;
   Invalid_Handle : constant Process_Handle;

   -------------------------
   -- Process Environment --
   -------------------------

   type Environ is limited private;
   --  Represent environment passed to a child process

   Case_Sensitive_Env_Var_Names : constant Boolean := False;

   procedure Inherit (Env : in out Environ);
   --  Call inherit to force parent process environment inheritance. When
   --  calling Inherit, all previous call to Set_Variable and/or Import are
   --  ignored.

   procedure Import (Env : in out Environ);
   --  Import current process environment into Env. Env can then be modified
   --  using Set_Variable.

   procedure Set_Variable
      (Env   : in out Environ;
       Name  : UTF8.UTF_8_String;
       Value : UTF8.UTF_8_String);
   --  Set variable Name to Value in Env

   function As_C (Env : Environ) return OS.C_WString;
   --  Export Env as a concatenation of null terminated wchar string followed
   --  by two wchar null characters. Each element is of the form "name=value"

   procedure Deallocate (Env : in out Environ);
   --  Deallocate Environ.

   -----------------------
   -- Process arguments --
   -----------------------

   type Arguments is limited private;
   --  Add an argument Arg to Args

   procedure Add_Argument
      (Args : in out Arguments;
       Arg  : UTF8.UTF_8_String);
   --  Add an argument Arg to Args

   function As_C (Args : Arguments) return OS.C_WString;
   --  Export Arguments as a null terminated wchar string

   procedure Deallocate (Args : in out Arguments);
   --  Deallocate Args

private

   package WSB renames GNATCOLL.WString_Builders;
   package WSLB renames GNATCOLL.WString_List_Builders;

   Invalid_Handle : constant Process_Handle := 0;

   type Arguments is new WSB.WString_Builder;

   type Environ is record
      Inherited : Boolean := False;
      Env       : WSLB.WString_List_Builder;
   end record;

end GNATCOLL.OS.Process_Types;
