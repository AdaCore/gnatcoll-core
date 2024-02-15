from e3.fs import mkdir
from e3.os.fs import touch
import os


def generate_test_trees(src_top_dir_name, dst_top_dir_name, symlink_to_missing_files):
    ###############
    # Source tree #
    ###############

    mkdir(src_top_dir_name)
    touch(os.path.join(src_top_dir_name, "file1"))

    if symlink_to_missing_files:
        os.symlink(
            src="./missing-file",
            dst=os.path.join(src_top_dir_name, "link_to_missing_file"),
        )
        os.symlink(
            src="./missing-file",
            dst=os.path.join(src_top_dir_name, "common_link_to_missing_file"),
        )
        os.symlink(
            src="/tmp/missing-file",
            dst=os.path.join(src_top_dir_name, "abs_link_to_missing_file"),
        )
        os.symlink(
            src="/tmp/missing-file",
            dst=os.path.join(src_top_dir_name, "common_abs_link_to_missing_file"),
        )
        os.symlink(
            src="/tmp/missing-file",
            dst=os.path.join(src_top_dir_name, "symlink_in_src_and_dir_in_dst"),
        )

    os.symlink(
        src="../" + src_top_dir_name + "/dir1",
        dst=os.path.join(src_top_dir_name, "relative_external_dir_link"),
    )

    mkdir(os.path.join(src_top_dir_name, "dir1"))
    mkdir(os.path.join(src_top_dir_name, "dir2"))
    mkdir(os.path.join(src_top_dir_name, "dir3"))

    dir1_path = os.path.join(src_top_dir_name, "dir1")
    touch(os.path.join(dir1_path, "file1"))
    os.symlink(src="../dir3", dst=os.path.join(dir1_path, "dir3_link"))

    dir3_path = os.path.join(src_top_dir_name, "dir3")
    touch(os.path.join(dir3_path, "file3"))
    os.symlink("./dir3/file3", os.path.join(src_top_dir_name, "file3_link"))

    ####################
    # Destination tree #
    ####################

    mkdir(dst_top_dir_name)
    mkdir(os.path.join(dst_top_dir_name, "dir1"))
    mkdir(os.path.join(dst_top_dir_name, "symlink_in_src_and_dir_in_dst"))

    # Create symbolic links which have the same name as in src
    os.symlink(
        src="./missing-file",
        dst=os.path.join(dst_top_dir_name, "common_link_to_missing_file"),
    )
    os.symlink(
        src="/tmp/missing-file",
        dst=os.path.join(dst_top_dir_name, "common_abs_link_to_missing_file"),
    )
    os.symlink(src="./dir1", dst=os.path.join(dst_top_dir_name, "dir2"))
    touch(os.path.join(dst_top_dir_name, "file"))
    os.symlink(src="./file", dst=os.path.join(dst_top_dir_name, "file1"))

    # Create symbolic link which only exists in dst
    os.symlink(
        src="./dir1", dst=os.path.join(dst_top_dir_name, "dir1_link_only_in_dst")
    )
    os.symlink(src="/tmp/", dst=os.path.join(dst_top_dir_name, "absolute_dir"))
    os.symlink(
        src="/tmp/missing-dir/",
        dst=os.path.join(dst_top_dir_name, "absolute_missing_dir"),
    )
    os.symlink(
        src="/tmp/abcdef", dst=os.path.join(dst_top_dir_name, "absolute_missing_file")
    )
