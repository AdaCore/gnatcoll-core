from e3.testsuite.result import TestStatus
from drivers.data_validation import DataValidationDriver
import os
import json
import logging


class JSONValidationDriver(DataValidationDriver):

    def validate_result(self, process, data_file, result):
        # Read data file
        with open(os.path.join(self.test_env['test_dir'], data_file),
                  encoding='utf-8') as fd:
            expected = json.load(fd)

        got = json.loads(process.out.decode('utf-8'))
        if got != expected:
            # Escape non-ASCII codepoints. This is necessary to avoid errors
            # when logging wide characters on ancient encodings, such as CP1252
            # on Windows.
            got = got.encode('unicode_escape').decode('ascii')
            expected = expected.encode('unicode_escape').decode('ascii')
            logging.debug('%s\n<=>\n%s', got, expected)
            result.set_status(TestStatus.FAIL)
            result.push_result()
        return True
