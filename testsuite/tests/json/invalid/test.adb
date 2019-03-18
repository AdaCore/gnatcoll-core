with Test_Assert;
with GNATCOLL.JSON;
with GNATCOLL.VFS;
with GNAT.Strings;
with Ada.Command_Line;
with Ada.Exceptions;

function Test return Integer is
   package A renames Test_Assert;
   package JSON renames GNATCOLL.JSON;
   package VFS renames GNATCOLL.VFS;

   procedure Test (JSON_String : String);

   procedure Test (JSON_String : String) is
   begin
      declare
         pragma Warnings (Off);
         V : JSON.JSON_Value;
         pragma Warning (On);
      begin
         V := JSON.Read (JSON_String);
         A.Assert (False, "invalid json (exception not raised)");
      exception
         when JSON.Invalid_JSON_Stream =>
            A.Assert (True, "failed with Invalid_JSON_Stream");
         when E : others =>
            A.Assert (False, "invalid json: (wrong exception)" & ASCII.LF &
                      Ada.Exceptions.Exception_Information (E));
      end;
   end Test;

   File_Content : GNAT.Strings.String_Access :=
      VFS.Read_File (VFS.Create (VFS."+" (Ada.Command_Line.Argument (1))));
begin
   Test (File_Content.all);
   GNAT.Strings.Free (File_Content);
   return A.Report;
end Test;
