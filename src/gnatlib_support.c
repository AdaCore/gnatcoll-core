
#ifdef HAVE_MMAP
#include <sys/mman.h>

int gnatlib_has_mmap() {
  return 1;
}

#else

int gnatlib_has_mmap() {
  return 0;
}

void *mmap(void *start, long length, int prot, int flags,
                  int fd, long offset)
{
  return (void*)0;
}

int munmap(void *start, long length) {
  return 0;
}

#endif
