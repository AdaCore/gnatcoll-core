import sys
sys.stdout.reconfigure(encoding='utf-8')
print(
    str(len(sys.argv[1:]))
    + "|"
    + "|".join([arg.replace("\n", "\\n") for arg in sys.argv[1:]])
)
