------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

package body GNATCOLL.IO is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (File_Record'Class, File_Access);

   ---------
   -- Ref --
   ---------

   procedure Ref (File : File_Access) is
   begin
      File.Ref_Count := File.Ref_Count + 1;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (File : in out File_Access) is
   begin
      if File.Ref_Count > 0 then
         File.Ref_Count := File.Ref_Count - 1;

         if File.Ref_Count = 0 then
            Destroy (File.all);
            Unchecked_Free (File);
         end if;
      end if;
   end Unref;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (File : in out File_Record) is
   begin
      Free (File.Full);
      Free (File.Normalized);
   end Destroy;

end GNATCOLL.IO;
