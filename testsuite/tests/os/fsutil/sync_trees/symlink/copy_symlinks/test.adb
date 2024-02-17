with Test_Assert;
with Ada.Text_IO;
with GNATCOLL.OS.FSUtil; use GNATCOLL.OS.FSUtil;
with Tree_Helper;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames Ada.Text_IO;

   Src : constant UTF8.UTF_8_String := "src_top_dir";
   Dst : constant UTF8.UTF_8_String := "dst_top_dir";
begin
   IO.Put_Line ("Test for GNATCOLL.OS.FSUtil.Sync_Trees");

   A.Assert (
     Sync_Trees
       (Src, Dst, Mode => TIMESTAMP, Symlink_Mode => COPY_SYMLINKS));

   Tree_Helper.Display_Trees (Src, Dst);
   Tree_Helper.Compare_Tree_Files_Content (Src, Dst);

   return A.Report;
end Test;
