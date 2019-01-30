with GNATCOLL.String_Builders;
with Test_Assert;
with Ada.Text_IO;

function Test return Integer is
   package SB renames GNATCOLL.String_Builders;
   package A renames Test_Assert;
   package IO renames Ada.Text_IO;

   function C_Len (Str : SB.CString) return Integer;
   pragma Import (C, C_Len, "c_strlen");
begin
   IO.Put_Line ("String_Builder tests");
   declare
      S             : SB.String_Builder;
      S_Size        : constant Integer := SB.String_Builder'Size / 8;
      S_Object_Size : constant Integer := SB.String_Builder'Object_Size / 8;
      Short_String  : constant String := "short string";
      Long_String   : constant String := "lonnnnnnnnnnnnnnnnnnnnnnnng string";
      C             : Character;
   begin
      IO.Put_Line ("String_Builder size:" & S_Size'Img);
      IO.Put_Line ("String_Builder object size:" & S_Object_Size'Img);
      A.Assert (SB.Length (S) = 0);
      A.Assert (SB.As_String (S) = "");
      A.Assert (C_Len (SB.As_CString (S)) = 0);
      SB.Append (S, Short_String);
      SB.Append (S, 'A');
      SB.Append (S, "");
      A.Assert (C_Len (SB.As_CString (S)) = Short_String'Length + 1);
      SB.Append (S, Long_String);
      SB.Append (S, 'B');
      SB.Append (S, Long_String);
      SB.Append (S, Long_String);
      A.Assert (SB.As_String (S),
                Short_String & 'A' & Long_String & 'B' & Long_String & Long_String);
      A.Assert (SB.Element (S, 7) = 's');
      A.Assert (SB.Length (S) = C_Len (SB.As_CString (S)));
      SB.Set (S, "hello");
      A.Assert (SB.As_String (S), "hello");
      A.Assert (SB.Element (S, 5) = 'o');
      A.Assert (SB.Length (S) = C_Len (SB.As_CString (S)));
      begin
         C := SB.Element (S, 6);
         A.Assert (False, "no exception");
      exception
         when Constraint_Error =>
            A.Assert (True);
         when others =>
            A.Assert (False, "wrong exception");
      end;
      SB.Deallocate (S);
   end;

   IO.Put_Line ("Static_String_Builder tests");
   declare
      S            : SB.Static_String_Builder (30 + 1);
      Short_String : constant String := "0123456789";
      C            : Character;
      S1           : SB.Static_String_Builder (1);
   begin
      A.Assert (SB.Length (S) = 0);
      A.Assert (SB.As_String (S), "");
      SB.Append (S, Short_String);
      SB.Append (S, Short_String);
      SB.Append (S, "");
      SB.Append (S, Short_String);
      A.Assert (SB.Length (S) = 30);
      A.Assert (SB.As_String (S), Short_String & Short_String & Short_String);

      SB.Set (S, "a");
      SB.Append (S, 'A');
      A.Assert (SB.Length (S) = 2);
      A.Assert (SB.As_String (S) = "aA");

      A.Assert (SB.Element (S, 2) = 'A');
      begin
         C := SB.Element (S, 3);
         A.Assert (False, "no exception");
      exception
         when Constraint_Error =>
            A.Assert (True);
         when others =>
            A.Assert (False, "wrong exception");
      end;

      begin
         SB.Append (S1, 'a');
         A.Assert (False, "no exception");
      exception
         when Constraint_Error =>
            A.Assert (True);
         when others =>
            A.Assert (False, "wrong exception");
      end;

      begin
         SB.Append (S1, "A");
         A.Assert (False, "no exception");
      exception
         when Constraint_Error =>
            A.Assert (True);
         when others =>
            A.Assert (False, "wrong exception");
      end;
      A.Assert (C_Len (SB.As_CString (S1)) = 0);
   end;
   return A.Report;
end Test;
