
#ifdef HAVE_MMAP
#include <sys/mman.h>

int gnatlib_has_mmap() {
  return 1;
}

#else

int gnatlib_has_mmap() {
  return 0;
}

void *mmap(void *start, size_t length, int prot, int flags,
                  int fd, off_t offset)
{
  return (NULL);
}

int munmap(void *start, size_t length) {
  return 0;
}

#endif
