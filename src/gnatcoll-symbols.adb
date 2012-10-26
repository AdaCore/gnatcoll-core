------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with Ada.Containers;             use Ada.Containers;
with Ada.Strings.Hash;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.IO;                    use GNAT.IO;
with GNAT.Strings;
with System.Address_Image;

package body GNATCOLL.Symbols is

   use String_Htable;

   Table_Size : constant := 98_317;
   --  The initial capacity of the htable. This was computed from inserting
   --  all entities from the GPS project, using Ada.Strings.Hash for the hash,
   --  but seems to be the same when using other hash codes.
   --  The table will readjust itself anyway, but setting this properly avoids
   --  a few resizing.

   -----------------
   -- Debug_Print --
   -----------------

   function Debug_Print (S : Symbol) return String is
   begin
      if S = No_Symbol then
         return "<Symbol: null>";
      else
         return "<Symbol: " & System.Address_Image (S.all'Address)
           & " {" & S.all & "}>";
      end if;
   end Debug_Print;

   ----------
   -- Hash --
   ----------

   function Hash (Str : Cst_String_Access) return Hash_Type is
   begin
      return Ada.Strings.Hash (Str.all);
   end Hash;

   ---------------
   -- Key_Equal --
   ---------------

   function Key_Equal (Key1, Key2 : Cst_String_Access) return Boolean is
   begin
      return Key1.all = Key2.all;
   end Key_Equal;

   ----------
   -- Find --
   ----------

   function Find
     (Table : access Symbol_Table_Record;
      Str   : String) return Symbol
   is
      Result : String_Htable.Cursor;
      Tmp    : Cst_String_Access;
   begin
      if Str'Length = 0 then
         return Empty_String;
      else
         Table.Calls_To_Find := Table.Calls_To_Find + 1;
         Result := Table.Hash.Find (Str'Unrestricted_Access);

         if not Has_Element (Result) then
            Table.Total_Size := Table.Total_Size + Str'Length;
            Tmp := new String'(Str);
            Table.Hash.Include (Tmp);
            return Symbol (Tmp);
         else
            Table.Size_Saved := Table.Size_Saved + Str'Length;
         end if;

         return Symbol (Element (Result));
      end if;
   end Find;

   -------------------
   -- Display_Stats --
   -------------------

   procedure Display_Stats (Self : access Symbol_Table_Record) is
      C     : String_Htable.Cursor := Self.Hash.First;
      Tmp   : Cst_String_Access;
      Count : Natural := 0;
      Last  : Hash_Type := Hash_Type'Last;
      H     : Hash_Type;

      Bucket_Count : Natural := 0;

   begin
      while Has_Element (C) loop
         Tmp := Element (C);

         H := Hash (Tmp);
         if H = Last then
            Count := Count + 1;
         else
            if Last /= Hash_Type'Last then
               Put_Line ("Bucket" & Last'Img & " =>" & Count'Img & " entries");
            end if;

            Last := H;
            Count := 1;
            Bucket_Count := Bucket_Count + 1;
         end if;

         Put_Line (Hash (Tmp)'Img & " => " & Tmp.all);
         Next (C);
      end loop;

      Put_Line ("Total calls to Find: " & Self.Calls_To_Find'Img);
      Put_Line ("Number of entries in the symbols table:"
                & Self.Hash.Length'Img);
      Put_Line ("Maximum number of buckets:" & Self.Hash.Capacity'Img);
      Put_Line ("Number of buckets used:" & Bucket_Count'Img);
      Put_Line ("Mean entries per bucket:"
                & Integer'Image (Integer (Self.Hash.Length) / Bucket_Count));
      Put_Line ("Total size in strings:" & Self.Total_Size'Img);
      Put_Line ("Size that would have been allocated for strings:"
                & Self.Size_Saved'Img);
   end Display_Stats;

   ---------
   -- Get --
   ---------

   function Get
     (Sym : Symbol; Empty_If_Null : Boolean := True) return Cst_String_Access
   is
   begin
      if Sym = No_Symbol then
         if Empty_If_Null then
            return Cst_String_Access (Empty_String);
         else
            return null;
         end if;
      else
         return Cst_String_Access (Sym);
      end if;
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (Table : in out Symbol_Table_Record) is
      function Convert is new Ada.Unchecked_Conversion
        (Cst_String_Access, GNAT.Strings.String_Access);

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (String, GNAT.Strings.String_Access);

      S   : GNAT.Strings.String_Access;
      C   : String_Htable.Cursor := Table.Hash.First;
      Tmp : Cst_String_Access;

   begin
      while Has_Element (C) loop
         Tmp := Element (C);
         Next (C);
         S := Convert (Tmp);
         Unchecked_Free (S);
      end loop;

      Table.Hash.Clear;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Table : in out Symbol_Table_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Symbol_Table_Record'Class, Symbol_Table_Access);
   begin
      if Table /= null then
         Free (Table.all);
         Unchecked_Free (Table);
      end if;
   end Free;

   ----------
   -- Hash --
   ----------

   function Hash (S : Symbol) return Hash_Type is
   begin
      return Hash (Cst_String_Access (S));
   end Hash;

   --------------
   -- Allocate --
   --------------

   function Allocate return Symbol_Table_Access is
      T : constant Symbol_Table_Access := new Symbol_Table_Record;
   begin
      T.Hash.Reserve_Capacity (Table_Size);
      return T;
   end Allocate;

end GNATCOLL.Symbols;
