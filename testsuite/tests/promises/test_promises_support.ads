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

with GNATCOLL.Promises; use GNATCOLL.Promises;

package Test_Promises_Support is

   Baseline : Integer := 0;
   Message : access String;
   --  Values that Assert routines should verify

   package Int_Promises is new Promises (Integer);
   package Float_Promises is new Promises (Float);
   package Str_Promises is new Promises (String);

   package Int_To_Float is new Chains (Int_Promises, Float_Promises);
   package Float_To_Str is new Chains (Float_Promises, Str_Promises);

   type Convert_Int is new Int_To_Float.Callback with null record;
   overriding procedure On_Next
      (Self   : in out Convert_Int;
       P      : Integer;
       Output : in out Float_Promises.Promise);

   type Convert_Float is new Float_To_Str.Callback with null record;
   overriding procedure On_Next
      (Self   : in out Convert_Float;
       P      : Float;
       Output : in out Str_Promises.Promise);

   type Display_String is new Str_Promises.Callback with null record;
   overriding procedure On_Next
      (Self   : in out Display_String;
       P      : String);
   overriding procedure On_Error
      (Self   : in out Display_String;
       Reason : String);

   type Display_Int is new Int_Promises.Callback with null record;
   overriding procedure On_Next
      (Self   : in out Display_Int;
       P      : Integer);

   type Fail_On_Float is new Float_To_Str.Callback with null record;
   overriding procedure On_Next
      (Self   : in out Fail_On_Float;
       P      : Float;
       Output : in out Str_Promises.Promise);
   --  Always fails Output

end Test_Promises_Support;
