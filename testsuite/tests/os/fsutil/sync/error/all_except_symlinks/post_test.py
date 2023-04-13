import os
import stat

# Restore write rights, so the script can remove directories after the tests
os.chmod(
    os.path.join("dst_file-only-in-dst", "dir1"),
    stat.S_IRWXU | stat.S_IRWXG | stat.S_IRWXO,
)
os.chmod(
    os.path.join("dst_dir-only-in-dst", "dir1"),
    stat.S_IRWXU | stat.S_IRWXG | stat.S_IRWXO,
)

os.chmod(os.path.join("dst_dir-vs-file", "dir1"), stat.S_IRUSR | stat.S_IXUSR)
os.chmod(os.path.join("dst_missing-dir-in-dst", "dir1"), stat.S_IRUSR | stat.S_IXUSR)
