import os
import shutil


# The symlink tests output files and directories encountered
# during the Walk procedure. The order of apparition is
# not deterministic, and depends on the underlying OS.
# This function sorts the "source tree" and "destination tree"
# so we can check the test output.
def sort_test_stdout():
    copy_file = open("tmp.out", "w")
    test_results_file = open("test_results.out", "r")
    results_content = test_results_file.readlines()

    source_tree_output = []
    destination_tree_output = []
    in_source_tree_output = False
    in_destination_tree_output = False

    for line in results_content:
        if "Source tree" in line:
            # Beginning of the source tree logs

            copy_file.write(line)
            in_source_tree_output = True

        elif "Destination tree" in line:
            # Beginning of the destination tree logs

            in_source_tree_output = False
            source_tree_output.sort()
            for l in source_tree_output:
                copy_file.write(l)

            copy_file.write(line)
            in_destination_tree_output = True

        elif "<==" in line:
            # Test results like:
            # <=== TEST PASSED ===>
            # We print the sorted destination tree content
            # before printing the test result.

            in_destination_tree_output = False
            destination_tree_output.sort()
            for l in destination_tree_output:
                copy_file.write(l)

            copy_file.write(line)

        elif "===" in line:
            # ===...== is below source tree and destination tree headers
            # we do not want it to be sorted.
            copy_file.write(line)

        elif in_source_tree_output:
            if "===" not in line:
                source_tree_output.append(line)
            else:
                copy_file.write(line)

        elif in_destination_tree_output:
            # ===...== is below source tree and destination tree headers
            # we do not want it to be sorted.
            if "===" not in line:
                destination_tree_output.append(line)
            else:
                copy_file.write(line)

        else:
            # We are neither in source nor in destination tree logs
            copy_file.write(line)

    copy_file.close()
    test_results_file.close()
    shutil.copyfile("tmp.out", "test_results.out")
    os.remove("tmp.out")
