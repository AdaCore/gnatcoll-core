import os
import stat

# Restore write rights, so the script can remove directories after the tests
os.chmod(
    os.path.join("dst_copy-symlinks", "dir1"),
    stat.S_IRWXU | stat.S_IRWXG | stat.S_IRWXO,
)
