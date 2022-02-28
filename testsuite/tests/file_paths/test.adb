with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Strings; use GNATCOLL.Strings;
with GNATCOLL.VFS;     use GNATCOLL.VFS;

with GNATCOLL.File_Paths; use GNATCOLL.File_Paths;

procedure Test is

   CWD : constant Virtual_File := Get_Current_Dir;

   function Filename_Image (Filename : String) return String;
   --  If ``Filename`` is absolute, transform it to make the test output stable

   Check_Components : constant XString_Array :=
     (To_XString ("bar"),
      To_XString ("foo"),
      To_XString ("dir1/bar"),
      To_XString ("dir1/foo"),
      To_XString ("dir2/foo"),
      To_XString (+CWD.Full_Name & "dir1/foo"));

   procedure Check (Path : String; Separator : Character; CWD : CWD_Mode);
   --  Parse ``Path`` with the given arguments and show the attempt to look up
   --  items in ``Check_Components`` on the resulting path.

   --------------------
   -- Filename_Image --
   --------------------

   function Filename_Image (Filename : String) return String is
      F : constant Virtual_File := Create (+Filename);

      function Canonicalize (S : String) return String;
      --  Canonicalize backslashes to forward slashes

      ------------------
      -- Canonicalize --
      ------------------

      function Canonicalize (S : String) return String is
      begin
         return R : String := S do
            for C of R loop
               if C = '\' then
                  C := '/';
               end if;
            end loop;
         end return;
      end Canonicalize;

   begin
      if F.Is_Absolute_Path then
         return "abs(""" & Canonicalize (+F.Relative_Path (CWD)) & """)";
      else
         return """" & Canonicalize (Filename) & """";
      end if;
   end Filename_Image;

   -----------
   -- Check --
   -----------

   procedure Check (Path : String; Separator : Character; CWD : CWD_Mode) is
      Label : constant String :=
        """" & Path & """, '" & Separator & "', " & CWD'Image;

      P : Any_Path;
   begin
      Put_Line (Label);
      Put_Line ((1 .. Label'Length => '='));
      New_Line;

      P := Parse_Path (Path, Separator, CWD);
      for C of Check_Components loop
         declare
            S : constant String := To_String (C);
         begin
            Put_Line
              (Filename_Image (S) & ": " & Filename_Image (Lookup (P, S)));
         end;
      end loop;

      New_Line;
   end Check;

begin
   --  The empty path is equivalent to the current working directory

   Check ("", ':', If_Empty);

   --  First items in the path have priority

   Check ("dir1|dir2", '|', If_Empty);
   Check ("dir2|dir1", '|', If_Empty);

   --  Check first/last CWD modes. The "bar" file is present both in "dir2" and
   --  in CWD, so its location should be different in the two cases below.

   Check ("dir2", '|', CWD_First);
   Check ("dir2", '|', CWD_Last);

   Put_Line ("Done.");
end Test;
