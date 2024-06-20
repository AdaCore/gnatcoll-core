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

with Ada.Environment_Variables;

package body GNATCOLL.OS.Process_Types is

   package Env_Vars renames Ada.Environment_Variables;

   ----------
   -- As_C --
   ----------

   function As_C (Args : Arguments) return C_String_Array is
   begin
      return SLB.As_C_String_Array (SLB.String_List_Builder (Args));
   end As_C;

   function As_C (Env : Environ) return C_String_Array is
      function GNAT_Environ return C_String_Array
      with Import        => True,
           Convention    => C,
           External_Name => "__gnat_environ";
   begin
      if Env.Inherited then
         return GNAT_Environ;
      else
         return SLB.As_C_String_Array (Env.Env);
      end if;
   end As_C;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (Args : in out Arguments) is
   begin
      SLB.Deallocate (SLB.String_List_Builder (Args));
   end Deallocate;

   procedure Deallocate (Env : in out Environ) is
   begin
      SLB.Deallocate (Env.Env);
      Env.Inherited := True;
   end Deallocate;

   ------------
   -- Import --
   ------------

   procedure Import (Env : in out Environ) is
      procedure Import_Var (Name, Value : String);

      procedure Import_Var (Name, Value : String) is
      begin
         Set_Variable (Env, Name, Value);
      end Import_Var;
   begin
      Env_Vars.Iterate (Import_Var'Unrestricted_Access);
   end Import;

   -------------
   -- Inherit --
   -------------

   procedure Inherit (Env : in out Environ) is
   begin
      Env.Inherited := True;
      SLB.Deallocate (Env.Env);
   end Inherit;

   -------------
   -- Program --
   -------------

   function Program (Args : Arguments) return UTF8.UTF_8_String
   is
   begin
      return SLB.Element (SLB.String_List_Builder (Args), 1);
   end Program;

   function Program (Args : Arguments) return C_String
   is
   begin
      return SLB.Element (SLB.String_List_Builder (Args), 1);
   end Program;

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable
      (Env   : in out Environ;
       Name  : UTF8.UTF_8_String;
       Value : UTF8.UTF_8_String)
   is
      Entry_Prefix : constant UTF8.UTF_8_String := Name & "=";
   begin

      --  First remove previous entry for variable Name
      if not Env.Inherited then
         for J in reverse 1 .. SLB.Length (Env.Env) loop
            declare
               El : constant String := SLB.Element (Env.Env, J);
            begin
               if El'Length >= Entry_Prefix'Length and then
                  El (El'First .. El'First + Entry_Prefix'Length - 1) =
                     Entry_Prefix
               then
                  SLB.Delete (Env.Env, J);

                  --  By construction we are sure the environment object cannot
                  --  contains twice an element starting with NAME= thus break
                  --  on first deletion.
                  exit;
               end if;
            end;
         end loop;
      end if;

      --  Add new entry
      SLB.Append (Env.Env, Name & "=" & Value);

      --  Environment is no more inherited
      Env.Inherited := False;
   end Set_Variable;

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument
      (Args : in out Arguments;
       Arg  : UTF8.UTF_8_String)
   is
   begin
      SLB.Append (SLB.String_List_Builder (Args), Arg);
   end Add_Argument;

end GNATCOLL.OS.Process_Types;
