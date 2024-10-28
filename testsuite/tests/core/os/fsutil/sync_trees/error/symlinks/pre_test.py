from e3.fs import mkdir
from e3.os.fs import touch
import os
import stat

#################
# Copy symlinks #
#################

src_top_dir_name = "src_copy-symlinks"
dst_top_dir_name = "dst_copy-symlinks"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
mkdir(os.path.join(src_top_dir_name, "dir1"))
touch(os.path.join(src_top_dir_name, "file1"))

mkdir(os.path.join(dst_top_dir_name, "dir1"))
os.symlink(
    src="../file1", dst=os.path.join(src_top_dir_name, "dir1", "symlink-to-file1")
)

# Protecting a directory can be done by setting its parent directory
# rights to read+execute, so we can still go to the directory and read
# its content but not modify it.
os.chmod(os.path.join(dst_top_dir_name, "dir1"), stat.S_IRUSR | stat.S_IXUSR)

############################
# Copy invalid file target #
############################

src_top_dir_name = "src_invalid-symlink-target"
dst_top_dir_name = "dst_invalid-symlink-target"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
mkdir(os.path.join(src_top_dir_name, "dir1"))
mkdir(os.path.join(dst_top_dir_name, "dir1"))

os.symlink(
    src="./missing-file", dst=os.path.join(src_top_dir_name, "dir1", "symlink-to-file1")
)

##############################################
# src dir and dst symlink with the same name #
##############################################

src_top_dir_name = "src_dir-vs-symlink"
dst_top_dir_name = "dst_dir-vs-symlink"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
mkdir(os.path.join(src_top_dir_name, "dir1"))
mkdir(os.path.join(dst_top_dir_name, "dir1"))

mkdir(os.path.join(src_top_dir_name, "dir1", "my_work"))
touch(os.path.join(src_top_dir_name, "dir1", "work"))

mkdir(os.path.join(dst_top_dir_name, "dir1", "work"))
os.symlink(src="./work", dst=os.path.join(dst_top_dir_name, "dir1", "my_work"))

os.chmod(os.path.join(dst_top_dir_name, "dir1"), stat.S_IRUSR | stat.S_IXUSR)

###############################################
# src file and dst symlink with the same name #
###############################################

src_top_dir_name = "src_file-vs-symlink"
dst_top_dir_name = "dst_file-vs-symlink"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
mkdir(os.path.join(src_top_dir_name, "dir1"))
mkdir(os.path.join(dst_top_dir_name, "dir1"))

touch(os.path.join(src_top_dir_name, "dir1", "my_work"))
touch(os.path.join(src_top_dir_name, "dir1", "work"))

touch(os.path.join(dst_top_dir_name, "dir1", "work"))
os.symlink(src="./work", dst=os.path.join(dst_top_dir_name, "dir1", "my_work"))

os.chmod(os.path.join(dst_top_dir_name, "dir1"), stat.S_IRUSR | stat.S_IXUSR)

##############################################
# src symlink and dst dir with the same name #
##############################################

src_top_dir_name = "src_symlink-vs-dir"
dst_top_dir_name = "dst_symlink-vs-dir"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
mkdir(os.path.join(src_top_dir_name, "dir1"))
mkdir(os.path.join(dst_top_dir_name, "dir1"))

mkdir(os.path.join(src_top_dir_name, "dir1", "work"))
os.symlink(src="./work", dst=os.path.join(src_top_dir_name, "dir1", "my_work"))

mkdir(os.path.join(dst_top_dir_name, "dir1", "my_work"))
mkdir(os.path.join(dst_top_dir_name, "dir1", "work"))

os.chmod(os.path.join(dst_top_dir_name, "dir1"), stat.S_IRUSR | stat.S_IXUSR)

###############################################
# src symlink and dst file with the same name #
###############################################

src_top_dir_name = "src_symlink-vs-file"
dst_top_dir_name = "dst_symlink-vs-file"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
mkdir(os.path.join(src_top_dir_name, "dir1"))
mkdir(os.path.join(dst_top_dir_name, "dir1"))

touch(os.path.join(src_top_dir_name, "dir1", "work"))
os.symlink(src="./work", dst=os.path.join(src_top_dir_name, "dir1", "my_work"))

touch(os.path.join(dst_top_dir_name, "dir1", "my_work"))
touch(os.path.join(dst_top_dir_name, "dir1", "work"))

os.chmod(os.path.join(dst_top_dir_name, "dir1"), stat.S_IRUSR | stat.S_IXUSR)

###########################################
# dst top directory is an invalid symlink #
###########################################

src_top_dir_name = "src_invalid-dst-top-dir-symlink"
dst_top_dir_name = "dst_invalid-dst-top-dir-symlink"

mkdir(src_top_dir_name)
os.symlink(src="./missing-file", dst=dst_top_dir_name)
