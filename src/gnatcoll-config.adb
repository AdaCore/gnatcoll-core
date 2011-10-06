-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2010-2011, AdaCore                  --
--                                                                   --
-- This is free software;  you can redistribute it and/or modify  it --
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

with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNATCOLL.Mmap;             use GNATCOLL.Mmap;
with GNATCOLL.Templates;        use GNATCOLL.Templates;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body GNATCOLL.Config is
   use String_Maps;

   function Internal_Get
     (Self    : Config_Pool;
      Key     : String;
      Section : String := Section_From_Key) return Config_Value;
   --  Internal version of Get

   function At_Index
     (Value : Config_Value; Index : Natural := Whole_Value) return String;
   --  Extract an element from a comma-separated list

   function Substitute
     (Self : INI_Parser'Class; Value : String) return String;
   --  Substitute various strings in the value read from the config file,
   --  for instance $HOME.

   -------------------
   -- Set_System_Id --
   -------------------

   procedure Set_System_Id (Self : in out Config_Parser; System_ID : String) is
   begin
      Self.System_ID := To_Unbounded_String (Normalize_Pathname (System_ID));
   end Set_System_Id;

   ----------------
   -- As_Integer --
   ----------------

   function As_Integer (Self : Config_Parser) return Integer is
   begin
      return Integer'Value (Value (Config_Parser'Class (Self)));
   end As_Integer;

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Self : Config_Parser) return Boolean is
   begin
      return Boolean'Value (Value (Config_Parser'Class (Self)));
   end As_Boolean;

   ----------------------
   -- As_Absolute_File --
   ----------------------

   function As_Absolute_File (Self : Config_Parser) return String is
      Val : constant String := Value (Config_Parser'Class (Self));
   begin
      if Val = "" then
         return "";
      elsif Val (Val'First) = '/' then
         return Val;
      else
         return Normalize_Pathname (Val, To_String (Self.System_ID));
      end if;
   end As_Absolute_File;

   ---------------------
   -- As_Absolute_Dir --
   ---------------------

   function As_Absolute_Dir (Self : Config_Parser) return String is
      V : constant String := As_Absolute_File (Config_Parser'Class (Self));
   begin
      if V = "" then
         return "";
      elsif V (V'Last) = Directory_Separator then
         return V;
      else
         return V & Directory_Separator;
      end if;
   end As_Absolute_Dir;

   ----------
   -- Open --
   ----------

   procedure Open (Self : in out File_Config_Parser; Filename : String) is
      F   : Mapped_File;
      Str : Str_Access;
   begin
      F := Open_Read (Filename);
      Read (F);
      Str := Data (F);

      Self.Contents := To_Unbounded_String (String (Str (1 .. Last (F))));
      Self.System_ID := To_Unbounded_String
        (Normalize_Pathname (Dir_Name (Filename)));
      Self.First    := 1;

      Close (F);
   end Open;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Self : File_Config_Parser) return Boolean is
   begin
      return Self.First > Length (Self.Contents);
   end At_End;

   ----------
   -- Open --
   ----------

   overriding procedure Open (Self : in out INI_Parser; Filename : String) is
   begin
      Open (File_Config_Parser (Self), Filename);
      Self.Eol      := 0;
      Next (Self);
   end Open;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out INI_Parser) is
      Eol   : Integer := Self.First;
      Tmp   : Integer;
      Last  : constant Integer := Length (Self.Contents);
      Comment : constant Integer := Length (Self.Comment_Start);
   begin
      Self.First := Self.Eol + 1;

      --  Search end of current line
      while Self.First <= Last loop
         Eol := Self.First;
         Self.Equal := 0;
         while Eol <= Last
           and then Element (Self.Contents, Eol) /= ASCII.LF
         loop
            if Self.Equal = 0 and then Element (Self.Contents, Eol) = '=' then
               Self.Equal := Eol;
            end if;
            Eol := Eol + 1;
         end loop;

         Self.Eol   := Eol;

         Tmp := Eol - 1;
         if Element (Self.Contents, Tmp) = ASCII.CR then
            Tmp := Tmp - 1;
         end if;

         --  Are we seeing a comment ?

         if Self.First + Comment - 1 <= Eol
           and then Slice
             (Self.Contents, Self.First, Self.First + Comment - 1) =
           Self.Comment_Start
         then
            null;

         elsif Self.Use_Sections
           and then Element (Self.Contents, Self.First) = '['
           and then Element (Self.Contents, Tmp) = ']'
         then
            Self.Current_Section := To_Unbounded_String
              (Strip_CR (Slice (Self.Contents, Self.First + 1, Tmp - 1)));

         elsif Self.Equal /= 0 then
            return;
         end if;

         Self.First := Eol + 1;
      end loop;
   end Next;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Self             : in out INI_Parser;
      Comment_Start    : String := "#";
      Handles_Sections : Boolean := True;
      Home             : String := "")
   is
   begin
      Self.Comment_Start := To_Unbounded_String (Comment_Start);
      Self.Use_Sections  := Handles_Sections;

      if Home /= "" then
         Self.Home := Create (+Home);
      end if;
   end Configure;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Self : INI_Parser'Class; Value : String) return String
   is
      function Callback (Name : String; Quoted : Boolean) return String;
      function Callback (Name : String; Quoted : Boolean) return String is
         pragma Unreferenced (Quoted);
      begin
         if Name = "HOME" then
            return +Self.Home.Full_Name;
         else
            raise Invalid_Substitution;
         end if;
      end Callback;

   begin
      return Substitute
        (Str       => Value,
         Callback  => Callback'Unrestricted_Access,
         Delimiter => '$');
   end Substitute;

   -------------
   -- Section --
   -------------

   overriding function Section (Self : INI_Parser) return String is
   begin
      return To_String (Self.Current_Section);
   end Section;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : INI_Parser) return String is
   begin
      return Trim
        (Strip_CR (Slice (Self.Contents, Self.First, Self.Equal - 1)),
         Side => Ada.Strings.Both);
   end Key;

   -----------
   -- Value --
   -----------

   overriding function Value (Self : INI_Parser) return String is
   begin
      return Substitute
        (Self,
         Trim (Strip_CR (Slice (Self.Contents, Self.Equal + 1, Self.Eol - 1)),
           Side => Ada.Strings.Left));
   end Value;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Self   : in out Config_Pool;
      Config : in out Config_Parser'Class)
   is
   begin
      Set_System_Id (Self, To_String (Config.System_ID));
      while not At_End (Config) loop
         Set (Self, Section (Config), Key (Config), Value (Config));
         Next (Config);
      end loop;
   end Fill;

   -------------------
   -- Set_System_Id --
   -------------------

   procedure Set_System_Id (Self : in out Config_Pool; System_ID : String) is
   begin
      Self.System_ID := To_Unbounded_String (Normalize_Pathname (System_ID));
   end Set_System_Id;

   ------------------
   -- Internal_Get --
   ------------------

   function Internal_Get
     (Self    : Config_Pool;
      Key     : String;
      Section : String := Section_From_Key) return Config_Value is
   begin
      if Section = Section_From_Key then
         for D in Key'Range loop
            if Key (D) = '.' then
               return Element (Self.Keys,
                               Key (Key'First .. D - 1)
                               & '#' & Key (D + 1 .. Key'Last));
            end if;
         end loop;

         return Element (Self.Keys, '#' & Key);
      else
         return Element (Self.Keys, Section & "#" & Key);
      end if;
   end Internal_Get;

   --------------
   -- At_Index --
   --------------

   function At_Index
     (Value : Config_Value; Index : Natural := Whole_Value) return String
   is
      S : String_List_Access;
   begin
      if Index = Whole_Value then
         return Value.Value;
      else
         S := Split (Value.Value, ',');

         if Index > S'Last then
            Free (S);
            return "";
         else
            return R : constant String := S (Index).all do
               Free (S);
            end return;
         end if;
      end if;
   end At_Index;

   ---------
   -- Get --
   ---------

   function Get
     (Self    : Config_Pool;
      Key     : String;
      Section : String := Section_From_Key;
      Index   : Natural := Whole_Value) return String
   is
   begin
      return At_Index (Internal_Get (Self, Key, Section), Index);
   end Get;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer
     (Self    : Config_Pool;
      Key     : String;
      Section : String := Section_From_Key;
      Index   : Natural := Whole_Value) return Integer is
   begin
      return Integer'Value (Get (Self, Key, Section, Index));
   end Get_Integer;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
     (Self    : Config_Pool;
      Key     : String;
      Section : String := Section_From_Key;
      Index   : Natural := Whole_Value) return Boolean is
   begin
      return Boolean'Value (Get (Self, Key, Section, Index));
   end Get_Boolean;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Self    : Config_Pool;
      Key     : String;
      Section : String := Section_From_Key;
      Index   : Natural := Whole_Value) return String
   is
      Val : constant Config_Value := Internal_Get (Self, Key, Section);
      V   : constant String := At_Index (Val, Index);
   begin
      if V = "" then
         return "";
      elsif Is_Absolute_Path (V) then
         return V;
      else
         return Normalize_Pathname (V, To_String (Val.System_ID));
      end if;
   end Get_File;

   -------------
   -- To_File --
   -------------

   function To_File
     (Self    : Config_Pool;
      Key     : String;
      Section : String := Section_From_Key;
      Value   : String) return String
   is
      Val : constant Config_Value := Internal_Get (Self, Key, Section);
   begin
      if Value = "" then
         return "";
      elsif Is_Absolute_Path (Value) then
         return Value;
      else
         return Normalize_Pathname (Value, To_String (Val.System_ID));
      end if;
   end To_File;

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out Config_Pool; Section, Key, Value : String) is
   begin
      Include (Self.Keys, Section & "#" & Key,
               Config_Value'
                 (Len       => Value'Length,
                  Value     => Value,
                  System_ID => Self.System_ID));
   end Set;

   ------------
   -- Create --
   ------------

   function Create (Key : String; Section : String := "") return Config_Key is
   begin
      return Config_Key'(Section => To_Unbounded_String (Section),
                         Key     => To_Unbounded_String (Key));
   end Create;

   ---------
   -- Get --
   ---------

   function Get
     (Self  : Config_Key;
      Conf  : Config_Pool'Class;
      Index : Natural := Whole_Value) return String is
   begin
      return Get (Conf, To_String (Self.Key), To_String (Self.Section), Index);
   end Get;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer
     (Self  : Config_Key;
      Conf  : Config_Pool'Class;
      Index : Natural := Whole_Value) return Integer is
   begin
      return Get_Integer
         (Conf, To_String (Self.Key), To_String (Self.Section), Index);
   end Get_Integer;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
     (Self  : Config_Key;
      Conf  : Config_Pool'Class;
      Index : Natural := Whole_Value) return Boolean is
   begin
      return Get_Boolean
         (Conf, To_String (Self.Key), To_String (Self.Section), Index);
   end Get_Boolean;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Self  : Config_Key;
      Conf  : Config_Pool'Class;
      Index : Natural := Whole_Value) return String is
   begin
      return Get_File
        (Conf, To_String (Self.Key), To_String (Self.Section), Index);
   end Get_File;

   -------------
   -- To_File --
   -------------

   function To_File
     (Self  : Config_Key;
      Conf  : Config_Pool'Class;
      Value : String) return String is
   begin
      return To_File
        (Conf, To_String (Self.Key), To_String (Self.Section), Value);
   end To_File;
end GNATCOLL.Config;
