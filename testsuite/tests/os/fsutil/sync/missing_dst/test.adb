with Test_Assert;
with GNAT.IO;
with GNATCOLL.OS.FSUtil; use GNATCOLL.OS.FSUtil;
with Ada.Strings.UTF_Encoding;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package UTF8 renames Ada.Strings.UTF_Encoding;

   Src : constant UTF8.UTF_8_String := "./src_top_dir";
   Dst : constant UTF8.UTF_8_String := "./dst_top_dir";
begin
   IO.Put_Line ("Test for GNATCOLL.OS.Dir");

   A.Assert (Sync_Trees (Src, Dst, Mode => TIMESTAMP));

   return A.Report;
end Test;
