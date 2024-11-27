from e3.testsuite.driver import TestDriver
from gprproject.testsuite.drivers import run_test_program

class GNATcollTestDriver(TestDriver):
    """Abstract class to share some common facilities."""

    DEFAULT_TIMEOUT = 5 * 60  # 5 minutes

    @property
    def process_timeout(self):
        """Timeout (in seconds) for subprocess to launch."""
        return self.test_env.get("timeout", self.DEFAULT_TIMEOUT)

    def run_test_program(self, cmd, slot, test_name=None, result=None, **kwargs):
        print(cmd)
        return run_test_program(self, cmd, slot, test_name, result, **kwargs)
