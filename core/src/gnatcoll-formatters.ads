------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

--  This package provides routines to format the text representation of complex
--  data structures.

with GNATCOLL.Strings_Impl;

package GNATCOLL.Formatters is

   generic
      with package Strings is new GNATCOLL.Strings_Impl.Strings (<>);
   procedure Columns_Vertical
     (Words     : Strings.XString_Array;
      Width     : Positive;
      Put_Line  : not null access procedure (Line : Strings.XString);
      Pad       : Strings.Char_Type   := Strings.Space;
      Delimiter : Strings.Char_String := (1 => Strings.Space));
   --  Procedure to format ordered phrases by columns vertically and output it
   --  to callback line by line.
   --  Number of columns limited by width and have to be calculated to minimize
   --  number of rows in each column. Output example:
   --
   --  A_2.0161E-01            L_8.34E-02                   W_3.4E-02
   --  B_4.112135470E-01       M_9.83147859573364258000E-01 X_1.232E-01
   --  C_1.8368E-01            N_4.7302106023E-01           Y_7.27181677268E-01
   --  D_2.25542E-01           O_8.702573776245117190E-01   Z_1.415E-01
   --  E_5.836335420609E-01    P_9.2786121368408203100E-01
   --  F_7.226370573043823E-01 Q_9.2725968360900878900E-01
   --  G_5.0E-03               R_7.7519929409027100E-01
   --  H_6.59124374389648E-01  S_2.48095E-01
   --  I_3.4402838E-01         T_9.0058088302612304700E-01
   --  J_1.7E-02               U_6.0053098201752E-01
   --  K_7.217630147933960E-01 V_8.02301645278930664E-01
   --
   --  Words is the array of strings to be formatted in the output.
   --  Width is the width limit of the output.
   --  Put_Line is the callback routine to take output line by line.
   --  Pad is the character filling the space after words to have same size in
   --  column for next column to be aligned.
   --  Delimiter is string delimiting the columns.

   generic
      with package Strings is new GNATCOLL.Strings_Impl.Strings (<>);
      End_Of_Line : Strings.Char_Type;
   function Columns_Vertical_XString
     (Words       : Strings.XString_Array;
      Width       : Positive;
      Pad         : Strings.Char_Type   := Strings.Space;
      Delimiter   : Strings.Char_String := (1 => Strings.Space))
      return Strings.XString;
   --  The same as above but returns formatted text at once

end GNATCOLL.Formatters;
