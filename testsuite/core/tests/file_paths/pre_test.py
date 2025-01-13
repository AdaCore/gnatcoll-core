from e3.os.fs import touch
from e3.fs import mkdir

if __name__ == "__main__":
    mkdir("foo")
    mkdir("dir1")
    mkdir("dir2")
    touch("dir1/foo")
    touch("dir2/foo")
    touch("dir2/bar")
    touch("bar")

