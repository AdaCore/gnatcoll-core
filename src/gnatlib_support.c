
#ifdef HAVE_MMAP
#include <sys/mman.h>

int gnatlib_has_mmap() {
  return 1;
}

void *gnatlib_mmap(void *start, long length, int prot, int flags,
                  int fd, long offset)
{
  return mmap (start, (size_t)length, prot, flags, fd, (off_t)offset);
}

int gnatlib_munmap(void *start, long length) {
  return munmap (start, (size_t)length);
}



#else

int gnatlib_has_mmap() {
  return 0;
}

void *gnatlib_mmap(void *start, long length, int prot, int flags,
                  int fd, long offset)
{
  return (void*)0;
}

int gnatlib_munmap(void *start, long length) {
  return 0;
}

#endif
