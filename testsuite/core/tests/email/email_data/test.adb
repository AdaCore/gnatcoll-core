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

with Ada.Calendar;
with Test_Assert;

with GNATCOLL.Email.Mailboxes;
with GNATCOLL.Email.Utils;
use  GNATCOLL.Email,
     GNATCOLL.Email.Mailboxes,
     GNATCOLL.Email.Utils;
with GNATCOLL.VFS; use GNATCOLL.VFS;

function Test return Integer is

   package Cal renames Ada.Calendar;
   package A renames Test_Assert;

   Count : Natural  := 0;

   procedure Parse_File (Filename : String);
   --  Parse file as mailbox

   procedure Parse_File (Filename : String) is
      Box    : Mbox;
      Msg    : Message;
      Addr   : Address_Set.Set;
      T      : Cal.Time;
      pragma Unreferenced (Addr, T);
   begin
      Open (Box, Filename => Create (+Filename));
      declare
         Curs : GNATCOLL.Email.Mailboxes.Cursor'Class := First (Box);
      begin
         while Has_Element (Curs) loop
            Get_Message (Curs, Box, Msg);
            if Msg /= Null_Message then
               Addr := Get_Recipients (Msg);
               T    := Date_From_Envelope (Msg);
            end if;
            Next (Curs, Box);
            Count := Count + 1;
         end loop;
      end;
   end Parse_File;

begin

   Parse_File ("tea_party.mbx");
   A.Assert (Count, 95, "expected number of messages");
   return A.Report;

end Test;
