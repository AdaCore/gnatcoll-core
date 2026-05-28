------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

--  Security tests for CRLF injection prevention in email headers and the
--  envelope From line.

with Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Email;        use GNATCOLL.Email;
with Test_Assert;

function Test return Integer is

   package A   renames Test_Assert;
   package ASF renames Ada.Strings.Fixed;

   LF  : constant Character := ASCII.LF;
   CR  : constant Character := ASCII.CR;
   Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   function Has_Substr (S : String; Sub : String) return Boolean;

   procedure Test_Envelope_Email_LF_Raises;
   procedure Test_Envelope_Email_CR_Raises;
   procedure Test_Envelope_Email_CRLF_Raises;
   procedure Test_Envelope_From_LF_Raises;
   procedure Test_Envelope_From_CRLF_Raises;
   procedure Test_Create_CR_Raises;
   procedure Test_Create_LF_Raises;
   procedure Test_Create_CRLF_Raises;
   procedure Test_Append_CR_Raises;
   procedure Test_Set_Param_CR_Raises;
   procedure Test_Default_Headers_Subject_Raises;
   procedure Test_Envelope_Email_NUL_Raises;
   procedure Test_Create_NUL_Raises;
   procedure Test_Envelope_From_NUL_Raises;
   procedure Test_Clean_Inputs_Work;

   function Has_Substr (S : String; Sub : String) return Boolean is
   begin
      return ASF.Index (S, Sub) /= 0;
   end Has_Substr;

   procedure Test_Envelope_Email_LF_Raises is
      Msg : Message := New_Message;
   begin
      Set_Envelope_From
        (Msg, "evil@example.com" & LF & "Bcc: attacker@evil.com", Now);
      A.Assert
        (False, "Forbidden_Character expected for LF in envelope email");
   exception
      when Forbidden_Character =>
         A.Assert
           (True, "Forbidden_Character raised for LF in envelope email");
   end Test_Envelope_Email_LF_Raises;

   procedure Test_Envelope_Email_CR_Raises is
      Msg : Message := New_Message;
   begin
      Set_Envelope_From
        (Msg, "evil@example.com" & CR & "Bcc: attacker@evil.com", Now);
      A.Assert
        (False, "Forbidden_Character expected for CR in envelope email");
   exception
      when Forbidden_Character =>
         A.Assert
           (True, "Forbidden_Character raised for CR in envelope email");
   end Test_Envelope_Email_CR_Raises;

   procedure Test_Envelope_Email_CRLF_Raises is
      Msg : Message := New_Message;
   begin
      Set_Envelope_From
        (Msg, "evil@example.com" & CR & LF & "Bcc: attacker@evil.com", Now);
      A.Assert
        (False, "Forbidden_Character expected for CRLF in envelope email");
   exception
      when Forbidden_Character =>
         A.Assert
           (True, "Forbidden_Character raised for CRLF in envelope email");
   end Test_Envelope_Email_CRLF_Raises;

   procedure Test_Envelope_From_LF_Raises is
      Msg : Message := New_Message;
   begin
      Set_Envelope_From
        (Msg, "From evil@example.com" & LF & "Bcc: attacker@evil.com");
      A.Assert (False, "Forbidden_Character expected for LF in envelope");
   exception
      when Forbidden_Character =>
         A.Assert (True, "Forbidden_Character raised for LF in envelope");
   end Test_Envelope_From_LF_Raises;

   procedure Test_Envelope_From_CRLF_Raises is
      Msg : Message := New_Message;
   begin
      Set_Envelope_From
        (Msg, "From evil@example.com" & CR & LF & "Bcc: attacker@evil.com");
      A.Assert (False, "Forbidden_Character expected for CRLF in envelope");
   exception
      when Forbidden_Character =>
         A.Assert (True, "Forbidden_Character raised for CRLF in envelope");
   end Test_Envelope_From_CRLF_Raises;

   procedure Test_Create_CR_Raises is
      H : Header;
      pragma Unreferenced (H);
   begin
      H := Create ("Subject", "Hello" & CR & "Bcc: attacker@evil.com");
      A.Assert (False, "Forbidden_Character expected for CR in Create value");
   exception
      when Forbidden_Character =>
         A.Assert (True, "Forbidden_Character raised for CR in Create value");
   end Test_Create_CR_Raises;

   procedure Test_Create_LF_Raises is
      H : Header;
      pragma Unreferenced (H);
   begin
      H := Create ("Subject", "Hello" & LF & "Bcc: attacker@evil.com");
      A.Assert (False, "Forbidden_Character expected for LF in Create value");
   exception
      when Forbidden_Character =>
         A.Assert (True, "Forbidden_Character raised for LF in Create value");
   end Test_Create_LF_Raises;

   procedure Test_Create_CRLF_Raises is
      H : Header;
      pragma Unreferenced (H);
   begin
      H := Create ("Subject", "Hello" & CR & LF & "Bcc: attacker@evil.com");
      A.Assert
        (False, "Forbidden_Character expected for CRLF in Create value");
   exception
      when Forbidden_Character =>
         A.Assert
           (True, "Forbidden_Character raised for CRLF in Create value");
   end Test_Create_CRLF_Raises;

   procedure Test_Append_CR_Raises is
      H : Header := Create ("Subject", "Hello");
   begin
      Append (H, CR & "Bcc: attacker@evil.com");
      A.Assert (False, "Forbidden_Character expected for CR in Append value");
   exception
      when Forbidden_Character =>
         A.Assert (True, "Forbidden_Character raised for CR in Append value");
   end Test_Append_CR_Raises;

   procedure Test_Set_Param_CR_Raises is
      H : Header := Create (Content_Type, "text/plain");
   begin
      Set_Param (H, "charset", "us-ascii" & CR & "Bcc: attacker@evil.com");
      A.Assert
        (False, "Forbidden_Character expected for CR in Set_Param value");
   exception
      when Forbidden_Character =>
         A.Assert
           (True, "Forbidden_Character raised for CR in Set_Param value");
   end Test_Set_Param_CR_Raises;

   procedure Test_Default_Headers_Subject_Raises is
      Msg : Message := New_Message;
   begin
      Set_Default_Headers
        (Msg,
         From_Email => "sender@example.com",
         Subject    => "Legit subject" & CR & LF & "Bcc: attacker@evil.com");
      A.Assert (False, "Forbidden_Character expected for CRLF in Subject");
   exception
      when Forbidden_Character =>
         A.Assert (True, "Forbidden_Character raised for CRLF in Subject");
   end Test_Default_Headers_Subject_Raises;

   procedure Test_Envelope_Email_NUL_Raises is
      Msg : Message := New_Message;
   begin
      Set_Envelope_From
        (Msg, "evil@example.com" & ASCII.NUL & "Bcc: attacker@evil.com", Now);
      A.Assert
        (False, "Forbidden_Character expected for NUL in envelope email");
   exception
      when Forbidden_Character =>
         A.Assert
           (True, "Forbidden_Character raised for NUL in envelope email");
   end Test_Envelope_Email_NUL_Raises;

   procedure Test_Create_NUL_Raises is
      H : Header;
      pragma Unreferenced (H);
   begin
      H := Create ("Subject", "Buy cheap meds" & ASCII.NUL & "Legit content");
      A.Assert (False, "Forbidden_Character expected for NUL in Create value");
   exception
      when Forbidden_Character =>
         A.Assert (True, "Forbidden_Character raised for NUL in Create value");
   end Test_Create_NUL_Raises;

   procedure Test_Envelope_From_NUL_Raises is
      Msg : Message := New_Message;
   begin
      Set_Envelope_From (Msg, "From evil@example.com" & ASCII.NUL & "extra");
      A.Assert (False, "Forbidden_Character expected for NUL in envelope");
   exception
      when Forbidden_Character =>
         A.Assert (True, "Forbidden_Character raised for NUL in envelope");
   end Test_Envelope_From_NUL_Raises;

   procedure Test_Clean_Inputs_Work is
      Msg    : Message := New_Message;
      Result : Unbounded_String;
   begin
      Set_Default_Headers
        (Msg,
         From_Email     => "sender@example.com",
         Subject        => "Hello world",
         From_Real_Name => "Alice");
      To_String (Msg, Envelope => False, Result => Result);
      A.Assert
        (Has_Substr (To_String (Result), "Subject: Hello world"),
         "clean subject appears in serialized message");
   end Test_Clean_Inputs_Work;

begin

   --  Set_Envelope_From (Email, Date) raises on injection
   Test_Envelope_Email_LF_Raises;
   Test_Envelope_Email_CR_Raises;
   Test_Envelope_Email_CRLF_Raises;

   --  Set_Envelope_From (From) also raises (parser uses RTrim_CR instead)
   Test_Envelope_From_LF_Raises;
   Test_Envelope_From_CRLF_Raises;

   --  Create raises on injection
   Test_Create_CR_Raises;
   Test_Create_LF_Raises;
   Test_Create_CRLF_Raises;

   --  Append raises on injection
   Test_Append_CR_Raises;

   --  Set_Param raises on injection
   Test_Set_Param_CR_Raises;

   --  Set_Default_Headers propagates exception from Create
   Test_Default_Headers_Subject_Raises;

   --  NUL injection
   Test_Envelope_Email_NUL_Raises;
   Test_Create_NUL_Raises;
   Test_Envelope_From_NUL_Raises;

   --  Normal usage still works
   Test_Clean_Inputs_Work;

   return A.Report;

end Test;
