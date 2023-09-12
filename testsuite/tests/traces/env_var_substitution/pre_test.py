#!/usr/bin/env python

import os
import glob


def unlink_if_exists(files):
    if isinstance(files, str):
        files = [files]
    for f in files:
        try:
            os.unlink(f)
        except OSError:
            pass

map(os.unlink, glob.glob("log_*"))
unlink_if_exists("tracename.pid")

