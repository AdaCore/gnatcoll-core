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
            try:
                got_str = json.dumps(got)
            except Exception:
                got_str = got
            try:
                expected_str = json.dumps(expected)
            except Exception:
                expected_str = expected
            got_str = got_str.encode('unicode_escape').decode('ascii')
            expected_str = expected_str.encode('unicode_escape').decode('ascii')
            logging.debug(f'Got: {got_str}\n<=>\nExp: {expected_str}')
            result.set_status(TestStatus.FAIL)
            self.push_result(result)
        return True
