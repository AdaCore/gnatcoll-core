import psutil
import os

print(psutil.Process(os.getpid()).nice())
