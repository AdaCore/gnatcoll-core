------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2013-2013, AdaCore                     --
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

package body GNATCOLL.Iconv is

   ----------------
   -- Iconv_Open --
   ----------------

   function Iconv_Open
      (To_Code         : String := UTF8;
       From_Code       : String := Locale;
       Transliteration : Boolean := False;
       Ignore          : Boolean := False) return Iconv_T
   is
      pragma Unreferenced (To_Code, From_Code, Transliteration, Ignore);
   begin
      return Iconv_T (System.Null_Address);
   end Iconv_Open;

   -----------
   -- Iconv --
   -----------

   procedure Iconv
      (State          : Iconv_T;
       Inbuf          : String;
       Input_Index    : in out Positive;
       Outbuf         : in out String;
       Output_Index   : in out Positive;
       Result         : out Iconv_Result)
   is
      pragma Unreferenced (State);
      Input_Length  : constant Natural := Inbuf'Last - Input_Index + 1;
      Output_Length : constant Natural := Outbuf'Last - Output_Index + 1;
   begin
      if Output_Length > Input_Length then
         Outbuf (Output_Index .. Output_Index + Input_Length - 1) :=
            Inbuf (Input_Index .. Inbuf'Last);
         Input_Index := Inbuf'Last + 1;
         Output_Index := Output_Index + Input_Length;
         Result := Success;

      else
         Outbuf (Output_Index .. Outbuf'Last) :=
            Inbuf (Input_Index .. Input_Index + Output_Length - 1);
         Input_Index := Input_Index + Output_Length;
         Output_Index := Outbuf'Last + 1;
         Result := Full_Buffer;
      end if;
   end Iconv;

   -----------
   -- Reset --
   -----------

   procedure Reset (State : Iconv_T) is
      pragma Unreferenced (State);
   begin
      null;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset
      (State        : Iconv_T;
       Outbuf       : in out String;
       Output_Index : in out Positive;
       Result       : out Iconv_Result)
   is
      pragma Unreferenced (State, Outbuf, Output_Index, Result);
   begin
      null;
   end Reset;

   -----------------
   -- Iconv_Close --
   -----------------

   procedure Iconv_Close (State : Iconv_T) is
      pragma Unreferenced (State);
   begin
      null;
   end Iconv_Close;

   -----------
   -- Iconv --
   -----------

   function Iconv (State : Iconv_T; Input : String) return String is
      pragma Unreferenced (State);
   begin
      return Input;
   end Iconv;

   -----------
   -- Iconv --
   -----------

   function Iconv
      (Input           : String;
       To_Code         : String := UTF8;
       From_Code       : String := Locale;
       Transliteration : Boolean := False;
       Ignore          : Boolean := False) return String
   is
      pragma Unreferenced (To_Code, From_Code, Transliteration, Ignore);
   begin
      return Input;
   end Iconv;

   ---------------
   -- Has_Iconv --
   ---------------

   function Has_Iconv return Boolean is
   begin
      return False;
   end Has_Iconv;

end GNATCOLL.Iconv;
