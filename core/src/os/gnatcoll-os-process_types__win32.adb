------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                   Copyright (C) 2021-2022, AdaCore                       --
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

with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Wide_Characters.Handling;
with Ada.Environment_Variables;
with GNATCOLL.String_Builders;

package body GNATCOLL.OS.Process_Types is

   package UTF renames Ada.Strings.UTF_Encoding.Wide_Strings;
   package SB renames GNATCOLL.String_Builders;
   package Env_Vars renames Ada.Environment_Variables;
   package WCH renames Ada.Wide_Characters.Handling;

   Minimal_Env : Environ;

   procedure Add_Minimal_Env (Env : in out Environ);

   procedure Add_Minimal_Env (Env : in out Environ) is
   begin
      if Env_Vars.Exists ("SYSTEMROOT") then
         WSLB.Append
            (Env.Env, "SYSTEMROOT=" & Env_Vars.Value ("SYSTEMROOT"));
      end if;
      if Env_Vars.Exists ("SYSTEMDRIVE") then
         WSLB.Append
            (Env.Env, "SYSTEMDRIVE=" & Env_Vars.Value ("SYSTEMDRIVE"));
      end if;
   end Add_Minimal_Env;

   ----------
   -- As_C --
   ----------

   function As_C (Args : Arguments) return OS.C_WString is
   begin
      return WSB.As_C_WString (WSB.WString_Builder (Args));
   end As_C;

   function As_C (Env : Environ) return OS.C_WString is
   begin
      if Env.Inherited then
         --  Returning a null environment will cause Win32 API to inherit the
         --  current environment.
         return Null_C_WString;
      elsif WSLB.Length (Env.Env) = 0 then
         return WSLB.As_C_WString (Minimal_Env.Env);
      else
         return WSLB.As_C_WString (Env.Env);
      end if;
   end As_C;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (Args : in out Arguments) is
   begin
      WSB.Deallocate (WSB.WString_Builder (Args));
   end Deallocate;

   procedure Deallocate (Env : in out Environ) is
   begin
      WSLB.Deallocate (Env.Env);
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
      WSLB.Deallocate (Env.Env);
   end Inherit;

   procedure Set_Variable
      (Env   : in out Environ;
       Name  : UTF8.UTF_8_String;
       Value : UTF8.UTF_8_String)
   is
      Entry_Prefix : constant Wide_String := UTF.Decode (Name & "=");
   begin
      --  First remove previous entry for variable Name.
      if not Env.Inherited then
         for J in reverse 1 .. WSLB.Length (Env.Env) loop
            declare
               El : constant Wide_String := WSLB.Element (Env.Env, J);
            begin
               if El'Length >= Entry_Prefix'Length and then
                  WCH.To_Lower
                     (El (El'First .. El'First + Entry_Prefix'Length - 1)) =
                  WCH.To_Lower (Entry_Prefix)
               then
                  WSLB.Delete (Env.Env, J);

                  --  By construction we are sure the environment object cannot
                  --  contains twice an element starting with NAME= thus break
                  --  on first deletion
                  exit;
               end if;
            end;
         end loop;
      end if;

      if WSLB.Length (Env.Env) = 0 then
         --  On Windows SYSTEMROOT variable is always needed. So keep the
         --  variable.
         Add_Minimal_Env (Env);
      end if;

      --  Add new entry
      WSLB.Append (Env.Env, Name & "=" & Value);
      Env.Inherited := False;
   end Set_Variable;

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument
      (Args : in out Arguments;
       Arg  : UTF8.UTF_8_String)
   is
      Quoted_Arg   : SB.Static_String_Builder (Arg'Length * 2 + 3 + 1);
      --  Quoted version of the argument

      Quote_Needed : Boolean  := False;
      --  Will be set to True if we need to append the quoted version
      --  of the argument (occurs when a tab or a space is present in
      --  the argument.

   begin
      --  Starting quote
      SB.Append (Quoted_Arg, '"');

      for K in Arg'Range loop

         if Arg (K) = '"' then
            --  If a quote is found escape it with backslash. If it was
            --  preceeded by backslashes then escape all of them.
            declare
               Index  : Integer := K - 1;
            begin
               while Index >= Arg'First and then Arg (Index) = '\' loop
                  SB.Append (Quoted_Arg, '\');
                  Index := Index - 1;
               end loop;
            end;

            SB.Append (Quoted_Arg, '\');
            SB.Append (Quoted_Arg, '"');

         elsif Arg (K) = ' ' or else Arg (K) = ASCII.HT then
            --  This is a tab or a space, so the argument should be quoted
            SB.Append (Quoted_Arg, Arg (K));
            Quote_Needed := True;
         else
            --  Other characters
            SB.Append (Quoted_Arg, Arg (K));
         end if;

      end loop;

      --  Closing quote
      SB.Append (Quoted_Arg, '"');

      declare
         Quoted_Arg_Str : constant String := SB.As_String (Quoted_Arg);
      begin
         --  If this is not the first argument add a space as separator
         if WSB.Length (WSB.WString_Builder (Args)) > 0 then
            WSB.Append (WSB.WString_Builder (Args), " ");
         end if;

         if Quote_Needed or else Arg'Length = 0 then
            --  Empty arguments and arguments containing spaces and tab should
            --  be quoted.
            WSB.Append (WSB.WString_Builder (Args), Quoted_Arg_Str);
         else
            --  Other arguments can be appended unchanged to the command line
            WSB.Append
               (WSB.WString_Builder (Args),
                Quoted_Arg_Str
                   (Quoted_Arg_Str'First + 1 .. Quoted_Arg_Str'Last - 1));
         end if;
      end;
   end Add_Argument;

begin
   Add_Minimal_Env (Minimal_Env);
end GNATCOLL.OS.Process_Types;
