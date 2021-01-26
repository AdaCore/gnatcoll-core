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

with GNATCOLL.String_List_Builders;
with Ada.Strings.UTF_Encoding;

package GNATCOLL.OS.Process_Types is

   package UTF8 renames Ada.Strings.UTF_Encoding;

   --  Unix process handle
   type Process_Handle is new Integer;
   Invalid_Handle : constant Process_Handle;

   --------------------------
   --  Process environment --
   --------------------------

   type Environ is limited private;
   --  Represent environment passed to a child process

   Case_Sensitive_Env_Var_Names : constant Boolean := True;

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
   --  Set variable Name to Value in Env.

   function As_C (Env : Environ) return C_String_Array;
   --  Export Env as an array of pointers to null terminated strings,
   --  terminated by a null pointer. Each element is of the form "name=value"

   procedure Deallocate (Env : in out Environ);
   --  Deallocate Env.

   -----------------------
   -- Process arguments --
   -----------------------

   type Arguments is limited private;
   --  Represent arguments, including the command used to spawn a process

   procedure Add_Argument
      (Args : in out Arguments;
       Arg  : UTF8.UTF_8_String);
   --  Add an argument Arg to Args.

   function Program (Args : Arguments) return UTF8.UTF_8_String;
   --  Return the first argument of Args which correspond to the program name

   function Program (Args : Arguments) return C_String;
   --  Return the first argument of Args which correspond to the program name

   function As_C (Args : Arguments) return C_String_Array;
   --  Export Env as an array of pointers to null terminated strings

   procedure Deallocate (Args : in out Arguments);
   --  Deallocate Args

private

   package SLB renames GNATCOLL.String_List_Builders;

   Invalid_Handle : constant Process_Handle := -1;

   type Arguments is new SLB.String_List_Builder;

   type Environ is record
      Inherited : Boolean := False;
      Env       : SLB.String_List_Builder;
   end record;

end GNATCOLL.OS.Process_Types;
