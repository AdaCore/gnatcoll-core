with GNATCOLL.String_List_Builders;
with GNATCOLL.OS;
with Test_Assert;
with Ada.Text_IO;

function Test return Integer is
   package SL renames GNATCOLL.String_List_Builders;
   package A renames Test_Assert;
   package IO renames Ada.Text_IO;

   function C_Len (Str : GNATCOLL.OS.C_String) return Integer
   with Import, Convention => C, External_Name => "c_strlen";

begin
   IO.Put_Line ("String_List_Builder tests");
   declare
      L : SL.String_List_Builder;
   begin
      for J in 1 .. 1000 loop
         SL.Append (L, J'Img);
      end loop;

      A.Assert (SL.Length (L), 1000);
      for J in 340 .. 350 loop
         A.Assert (SL.Element (L, J), J'Img);
      end loop;

      A.Assert (C_Len (SL.Element (L, 10)), 3);

      SL.Delete (L, 5);

      A.Assert (SL.Element (L, 5), " 6");

      SL.Delete (L, 999);
      A.Assert (SL.Element (L, 8), " 9");
      A.Assert (SL.Element (L, 998), " 999");

      SL.Deallocate (L);

      A.Assert (SL.Length (L), 0);
   end;
   return A.Report;
end Test;
