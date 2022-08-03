with GNATCOLL.OS.FSUtil; use GNATCOLL.OS.FSUtil;
with Test_Assert;
with Ada.Text_IO;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames Ada.Text_IO;

begin
   IO.Put_Line ("GNATCOLL.OS.FSUtil test");

   declare
      SHA1_Str : constant String := String (SHA1 ("./data.txt"));
      SHA256_Str : constant String := String (SHA256 ("./data.txt"));
   begin
      A.Assert
         (SHA1_Str, "0a963bb418c97dff49ec8d166834ee23a912a0e9", "Check sha1");
      A.Assert
         (SHA256_Str,
          "93827371a7c9502512672999a530fb55999b054d4a05af3c2c02290bdded0d4c",
          "Check sha256");
   end;
   return A.Report;
end Test;
