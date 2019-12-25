------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

with Ada.Text_IO;     use Ada.Text_IO;

with Test_Assert;

package body Test_Promises_Support is

   package A renames Test_Assert;

   overriding procedure On_Next
      (Self   : in out Convert_Int;
       P      : Integer;
       Output : in out Float_Promises.Promise)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Convert_Int.On_Next  input=" & P'Img);
      A.Assert (P, Baseline, "expected input");
      Output.Set_Value (Float (P));
   end On_Next;

   overriding procedure On_Next
      (Self   : in out Convert_Float;
       P      : Float;
       Output : in out Str_Promises.Promise)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Convert_Float.On_Next  input=" & P'Img);
      A.Assert (Integer (P), Baseline, "expected input");
      Output.Set_Value ("value was" & P'Img);
   end On_Next;

   overriding procedure On_Next
      (Self   : in out Display_Int;
       P      : Integer)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Display_Int.On_Next  input=" & P'Img);
      A.Assert (P, Baseline, "expected input");
   end On_Next;

   overriding procedure On_Next
      (Self   : in out Display_String;
       P      : String)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Display_String.On_Next  input=" & P);
      A.Assert (P, "value was 2.00000E+00", "expected input");
   end On_Next;

   overriding procedure On_Error
      (Self   : in out Display_String;
       Reason : String)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Display_String.Failed because " & Reason);
      A.Assert (Reason, Message.all, "expected reason");
   end On_Error;

   overriding procedure On_Next
      (Self   : in out Fail_On_Float;
       P      : Float;
       Output : in out Str_Promises.Promise)
   is
      pragma Unreferenced (Self, P);
   begin
      Put_Line ("Fail_On_Float: mark output as failed");
      Output.Set_Error ("explicit");
   end On_Next;

end Test_Promises_Support;
