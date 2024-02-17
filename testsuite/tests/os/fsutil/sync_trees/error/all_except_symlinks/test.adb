with Test_Assert;
with GNAT.IO;
with GNATCOLL.OS.FSUtil; use GNATCOLL.OS.FSUtil;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames GNAT.IO;
begin
   IO.Put_Line ("Test for GNATCOLL.OS.FSUtil");

   A.Assert (not Sync_Trees
     ("src_diff-file-content", "dst_diff-file-content", Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_file-only-in-dst", "dst_file-only-in-dst", Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_dir-only-in-dst", "dst_dir-only-in-dst", Mode => CHECKSUM));

  --   A.Assert (not Sync_Trees
  --     ("src_dir-different-ts", "dst_dir-different-ts", Mode => TIMESTAMP));

   A.Assert (not Sync_Trees
     ("src_dir-vs-file", "dst_dir-vs-file", Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_file-vs-dir", "dst_file-vs-dir", Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_missing-dst-top-dir", "dst_missing-dst-top-dir/dir1/missing-dir",
      Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_missing-dir-in-dst", "dst_missing-dir-in-dst", Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("missing-src-dir", "dst_missing-src-dir", Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_single-file-in-src", "dst_single-file-in-src", Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_single-file-in-dst", "dst_single-file-in-dst", Mode => CHECKSUM));

   return A.Report;
end Test;