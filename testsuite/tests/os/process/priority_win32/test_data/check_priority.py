import psutil
import os

print(int(psutil.Process(os.getpid()).nice()))
