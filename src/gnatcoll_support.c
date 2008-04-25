#include <sys/stat.h>

#ifdef _WIN32
#include <windows.h>
#endif

#ifdef HAVE_MMAP
#include <sys/mman.h>

int gnatcoll_has_mmap() {
  return 1;
}

void *gnatcoll_mmap(void *start, long length, int prot, int flags,
                  int fd, long offset)
{
  return mmap (start, (size_t)length, prot, flags, fd, (off_t)offset);
}

int gnatcoll_munmap(void *start, long length) {
  return munmap (start, (size_t)length);
}



#else

int gnatcoll_has_mmap() {
  return 0;
}

void *gnatcoll_mmap(void *start, long length, int prot, int flags,
                  int fd, long offset)
{
  return (void*)0;
}

int gnatcoll_munmap(void *start, long length) {
  return 0;
}

#endif

int
__gnatcoll_get_logical_drive_strings (char *buffer, int len)
{
#ifdef _WIN32
  return GetLogicalDriveStringsA ((DWORD)len, (LPSTR)buffer);
#else
  return 0;
#endif
}

void
__gnatcoll_set_readable (char *file, int set)
{
  struct stat statbuf;

  if (!__gnat_stat (file, &statbuf))
    {
      if (set)
        chmod (file, statbuf.st_mode | S_IREAD);
      else
        chmod (file, statbuf.st_mode & (~S_IREAD));
    }
}

