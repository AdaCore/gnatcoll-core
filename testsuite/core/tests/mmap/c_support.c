#include <stdint.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>

#if defined(_WIN32) || defined(__MINGW32__)

#include <io.h>

#define OPEN _open
#define CLOSE _close
#define WRITE _write

/* We don't use standard lseek implementation on Windows otherwise
 * creating very large file will be inefficient as mingw lseek will
 * not create a sparse file. */
#define LSEEK _lseeki64

#define OPEN_FLAGS (_O_CREAT | _O_WRONLY | _O_TRUNC | _O_BINARY)
#define OPEN_MODE (_S_IREAD | _S_IWRITE)

#else

#include <unistd.h>

#define OPEN open
#define CLOSE close
#define WRITE write
#define LSEEK lseek

#define OPEN_FLAGS (O_CREAT | O_WRONLY | O_TRUNC)
#define OPEN_MODE 0644
#endif

/* See test.adb */
int
c_create_file (char *filename, int64_t offset)
{
   int fd = OPEN (filename, OPEN_FLAGS, OPEN_MODE);
   if (fd < 0)
      {
         perror ("cannot create file");
         return 0;
      }

   if (LSEEK (fd, offset - 3, SEEK_SET) != offset - 3)
      {
         perror ("cannot lseek");
         CLOSE (fd);
         return 0;
      }

   if (WRITE (fd, "xyz", 3) != 3)
      {
         perror ("cannot write");
         CLOSE (fd);
         return 0;
      }

   CLOSE (fd);
   return 1;
}
