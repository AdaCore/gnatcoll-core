#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>

/* We don't use standard lseek implementation on Windows otherwise
 * creating very large file will be inefficient as mingw lseek will
 * not create a sparse file. */
#if defined (_WIN32) || defined (__MINGW32__)
#define LSEEK _lseeki64
#else
#define LSEEK lseek
#endif

/* See test.adb */
int c_create_file (char* filename, int64_t offset)
{
    int fd = creat (filename, 0644);
    if (fd < 0) {
        printf("cannot create file\n");
        return 0;
    }

    if (LSEEK (fd, offset - 3, SEEK_SET) != offset - 3) {
        printf("cannot lseek\n");
        close (fd);
        return 0;
    }

    if (write (fd, "xyz", 3) != 3) {
        printf("cannot write\n");
        close (fd);
        return 0;
    }
    close (fd);
    return 1;
}
