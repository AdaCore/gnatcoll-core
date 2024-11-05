import os
import sys

sys.path.insert(1, os.path.join(sys.path[0], "../"))
sys.path.insert(1, os.path.join(sys.path[0], "../../"))
from test_tree_generator import generate_test_trees
from settings import src_top_dir_name, dst_top_dir_name

generate_test_trees(src_top_dir_name, dst_top_dir_name, True)
