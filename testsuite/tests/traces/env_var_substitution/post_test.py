#!/usr/bin/env python

import glob


pid = int(open('tracename.pid').readline())
for pat in ['log_pid.%d' % pid,
            'log_D.[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]',
            'log_T.[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]' +
            'T[0-9][0-9][0-9][0-9][0-9][0-9]',
            'log_FOO.BAR']:
    files = glob.glob(pat)
    if len(files) != 1:
        print("expected 1 file matching %s, found %d"
              % (pat, len(files)))
    fnam = files[0]
    if "hello from %d" % pid not in open(fnam).readline():
        print("Marker not found in %s" % fnam)
