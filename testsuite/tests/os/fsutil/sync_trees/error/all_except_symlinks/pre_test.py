from e3.fs import mkdir
from e3.os.fs import touch
import os
import stat

##########################################
# File content different in dst and src. #
##########################################

src_top_dir_name = "src_diff-file-content"
dst_top_dir_name = "dst_diff-file-content"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
touch(os.path.join(src_top_dir_name, "file-1"))

dst_fd = open(os.path.join(dst_top_dir_name, "file-1"), "w")
dst_fd.write("Content not present in src file")

# Remove the write right, so the copy from src to dst will fail
os.chmod(os.path.join(dst_top_dir_name, "file-1"), stat.S_IRUSR | stat.S_IXUSR)

####################
# File only in dst #
####################

src_top_dir_name = "src_file-only-in-dst"
dst_top_dir_name = "dst_file-only-in-dst"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
mkdir(os.path.join(src_top_dir_name, "dir1"))
dst_dir1_name = os.path.join(dst_top_dir_name, "dir1")
mkdir(dst_dir1_name)

touch(os.path.join(dst_dir1_name, "file-1"))

# Protecting a file can be done by setting its directory
# rights to read-only.
os.chmod(os.path.join(dst_dir1_name), stat.S_IRUSR | stat.S_IXUSR)


###################
# Dir only in dst #
###################

src_top_dir_name = "src_dir-only-in-dst"
dst_top_dir_name = "dst_dir-only-in-dst"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
mkdir(os.path.join(src_top_dir_name, "dir1"))
mkdir(os.path.join(dst_top_dir_name, "dir1"))

# Protecting a directory can be done by setting its parent directory
# rights to read+execute, so we can still go to the directory and read
# its content but not modify it.
mkdir(os.path.join(dst_top_dir_name, "dir1", "subdir1"))
os.chmod(os.path.join(dst_top_dir_name, "dir1"), stat.S_IRUSR | stat.S_IXUSR)

###########################################
# src dir and dst file with the same name #
###########################################

src_top_dir_name = "src_dir-vs-file"
dst_top_dir_name = "dst_dir-vs-file"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
mkdir(os.path.join(src_top_dir_name, "dir1"))
mkdir(os.path.join(dst_top_dir_name, "dir1"))

mkdir(os.path.join(src_top_dir_name, "dir1", "my_work"))
touch(os.path.join(dst_top_dir_name, "dir1", "my_work"))
os.chmod(os.path.join(dst_top_dir_name, "dir1"), stat.S_IRUSR | stat.S_IXUSR)

###########################################
# src file and dst dir with the same name #
###########################################

src_top_dir_name = "src_file-vs-dir"
dst_top_dir_name = "dst_file-vs-dir"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
mkdir(os.path.join(src_top_dir_name, "dir1"))
mkdir(os.path.join(dst_top_dir_name, "dir1"))

mkdir(os.path.join(dst_top_dir_name, "dir1", "my_work"))
touch(os.path.join(src_top_dir_name, "dir1", "my_work"))

os.chmod(os.path.join(dst_top_dir_name, "dir1"), stat.S_IRUSR | stat.S_IXUSR)

###################################################################
# Missing top dst directory, without write access for the dst dir #
###################################################################

src_top_dir_name = "src_missing-dst-top-dir"
dst_top_dir_name = "dst_missing-dst-top-dir"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
mkdir(os.path.join(src_top_dir_name, "dir1"))
mkdir(os.path.join(dst_top_dir_name, "dir1"))
os.chmod(os.path.join(dst_top_dir_name, "dir1"), stat.S_IRUSR | stat.S_IXUSR)

############################
# Directory missing in dst #
############################

src_top_dir_name = "src_missing-dir-in-dst"
dst_top_dir_name = "dst_missing-dir-in-dst"

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
mkdir(os.path.join(src_top_dir_name, "dir1"))
mkdir(os.path.join(dst_top_dir_name, "dir1"))

mkdir(os.path.join(src_top_dir_name, "dir1", "subdir"))
os.chmod(os.path.join(dst_top_dir_name, "dir1"), stat.S_IRUSR | stat.S_IXUSR)

############################
# Missing source directory #
############################

dst_top_dir_name = "dst_missing-src-dir"

mkdir(dst_top_dir_name)

################################
# Unsupported single file copy #
################################

src_top_dir_name = "src_single-file-in-src"
dst_top_dir_name = "dst_single-file-in-src"

touch(src_top_dir_name)
mkdir(dst_top_dir_name)

src_top_dir_name = "src_single-file-in-dst"
dst_top_dir_name = "dst_single-file-in-dst"

mkdir(src_top_dir_name)
touch(dst_top_dir_name)
