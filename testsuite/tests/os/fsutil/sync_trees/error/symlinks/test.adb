with Test_Assert;
with GNAT.IO;
with GNATCOLL.OS.FSUtil; use GNATCOLL.OS.FSUtil;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames GNAT.IO;
begin
   IO.Put_Line ("Test for GNATCOLL.OS.FSUtil");

   A.Assert (not Sync_Trees
     ("src_copy-symlinks", "dst_copy-symlinks",
      Symlink_Mode => COPY_SYMLINKS, Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_copy-symlinks", "dst_copy-symlinks",
      Symlink_Mode => COPY_SAFE_SYMLINKS, Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_copy-symlinks", "dst_copy-symlinks",
      Symlink_Mode => COPY_UNSAFE_SYMLINKS, Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_copy-symlinks", "dst_copy-symlinks",
      Symlink_Mode => COPY_UNSAFE_SYMLINKS, Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_invalid-symlink-target", "dst_invalid-symlink-target",
      Symlink_Mode => COPY_SYMLINKS_TARGET, Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_dir-vs-symlink", "dst_dir-vs-symlink",
      Symlink_Mode => COPY_SYMLINKS_TARGET, Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_file-vs-symlink", "dst_file-vs-symlink",
      Symlink_Mode => COPY_SYMLINKS_TARGET, Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_symlink-vs-file", "dst_symlink-vs-file",
      Symlink_Mode => SKIP_SYMLINKS, Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_symlink-vs-dir", "dst_symlink-vs-dir",
      Symlink_Mode => SKIP_SYMLINKS, Mode => CHECKSUM));

   A.Assert (not Sync_Trees
     ("src_invalid-dst-top-dir-symlink", "dst_invalid-dst-top-dir-symlink",
      Symlink_Mode => SKIP_SYMLINKS, Mode => CHECKSUM));

   return A.Report;
end Test;