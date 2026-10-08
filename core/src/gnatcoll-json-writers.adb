------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2026, AdaCore                      --
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

with GNATCOLL.JSON.Utility;

package body GNATCOLL.JSON.Writers is

   procedure Before_Value (Self : in out Writer);
   procedure End_Container (Self : in out Writer; Kind : Container_Kind);
   procedure Quote is new Utility.Emit_String (Append);

   ------------------
   -- Before_Value --
   ------------------

   procedure Before_Value (Self : in out Writer) is
   begin
      if Self.Finished then
         raise Writer_Error with "document already finished";
      elsif Self.Stack.Is_Empty then
         if Self.Root_Written then
            raise Writer_Error with "multiple root values";
         end if;
         Self.Root_Written := True;
      else
         declare
            Top : Frame renames Self.Stack.Reference (Self.Stack.Last_Index);
         begin
            if Top.Kind = Object_Kind then
               if not Top.Name_Pending then
                  raise Writer_Error with "object member name required";
               end if;
               Top.Name_Pending := False;
            else
               if not Top.First then
                  Append (",");
               end if;
               Top.First := False;
            end if;
         end;
      end if;
   end Before_Value;

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array (Self : in out Writer) is
   begin
      End_Container (Self, Array_Kind);
   end End_Array;

   -------------------
   -- End_Container --
   -------------------

   procedure End_Container (Self : in out Writer; Kind : Container_Kind) is
   begin
      if Self.Finished or else Self.Stack.Is_Empty then
         raise Writer_Error with "no open container";
      elsif Self.Stack.Last_Element.Kind /= Kind then
         raise Writer_Error with "mismatched container";
      elsif Self.Stack.Last_Element.Name_Pending then
         raise Writer_Error with "object member value required";
      end if;
      Append ((if Kind = Array_Kind then "]" else "}"));
      Self.Stack.Delete_Last;
   end End_Container;

   ----------------
   -- End_Object --
   ----------------

   procedure End_Object (Self : in out Writer) is
   begin
      End_Container (Self, Object_Kind);
   end End_Object;

   ------------
   -- Finish --
   ------------

   procedure Finish (Self : in out Writer) is
   begin
      if not Self.Root_Written or else not Self.Stack.Is_Empty then
         raise Writer_Error with "incomplete document";
      end if;
      Self.Finished := True;
   end Finish;

   ----------
   -- Name --
   ----------

   procedure Name (Self : in out Writer; Text : UTF8_String) is
   begin
      if Self.Finished or else Self.Stack.Is_Empty then
         raise Writer_Error with "name outside object";
      end if;
      declare
         Top : Frame renames Self.Stack.Reference (Self.Stack.Last_Index);
      begin
         if Top.Kind /= Object_Kind or else Top.Name_Pending then
            raise Writer_Error with "unexpected object member name";
         end if;
         if not Top.First then
            Append (",");
         end if;
         Quote (Text);
         Append (":");
         Top.First := False;
         Top.Name_Pending := True;
      end;
   end Name;

   ----------------
   -- Null_Value --
   ----------------

   procedure Null_Value (Self : in out Writer) is
   begin
      Before_Value (Self);
      Append ("null");
   end Null_Value;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Writer) is
   begin
      Self.Stack.Clear;
      Self.Root_Written := False;
      Self.Finished := False;
   end Reset;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : in out Writer) is
   begin
      Before_Value (Self);
      Append ("[");
      Self.Stack.Append (Frame'(Kind => Array_Kind, others => <>));
   end Start_Array;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : in out Writer) is
   begin
      Before_Value (Self);
      Append ("{");
      Self.Stack.Append (Frame'(Kind => Object_Kind, others => <>));
   end Start_Object;

   -----------
   -- Value --
   -----------

   procedure Value (Self : in out Writer; Item : UTF8_String) is
   begin
      Before_Value (Self);
      Quote (Item);
   end Value;

   procedure Value (Self : in out Writer; Item : UTF8_Unbounded_String) is
   begin
      Self.Value (Ada.Strings.Unbounded.To_String (Item));
   end Value;

   procedure Value (Self : in out Writer; Item : UTF8_XString) is
      Text   : GNATCOLL.Strings.Char_Array;
      Length : Natural;
   begin
      Item.Get_String (Text, Length);
      Self.Value (String (Text (1 .. Length)));
   end Value;

   procedure Value (Self : in out Writer; Item : Boolean) is
   begin
      Before_Value (Self);
      Append ((if Item then "true" else "false"));
   end Value;

   procedure Value (Self : in out Writer; Item : Long_Long_Integer) is
      Image : constant String := Item'Image;
   begin
      Before_Value (Self);
      Append (Image ((if Image (Image'First) = ' '
                      then Image'First + 1 else Image'First) .. Image'Last));
   end Value;

   procedure Value (Self : in out Writer; Item : Long_Float) is
      Image : constant String := Item'Image;
   begin
      for C of Image loop
         if C not in '0' .. '9' | ' ' | '-' | '+' | '.' | 'E' then
            raise Writer_Error with "non-finite number";
         end if;
      end loop;
      Before_Value (Self);
      Append (Image ((if Image (Image'First) = ' '
                      then Image'First + 1 else Image'First) .. Image'Last));
   end Value;

end GNATCOLL.JSON.Writers;
