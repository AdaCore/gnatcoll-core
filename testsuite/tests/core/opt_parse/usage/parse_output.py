import re
with open("stdout.log", "r") as f:
   s = " ".join(map(lambda x: x.strip(), f.readlines()))
with open("stdout.log", "w") as f:
   f.write(s)