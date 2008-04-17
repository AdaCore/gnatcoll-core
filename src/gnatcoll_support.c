
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
