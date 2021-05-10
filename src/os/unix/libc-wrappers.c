/*----------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

/* Ensure 64bits file operations are available */
#define _LARGE_FILE_SOURCE 1
#define _FILE_OFFSET_BITS 64
#define _GNU_SOURCE 1

#include <sys/stat.h>
#include <sys/statvfs.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <spawn.h>
#include <unistd.h>

typedef unsigned long long int uint_64;
typedef unsigned int uint_32;
typedef long long int sint_64;

/* Simpler mapping of libc statvfs struct. This structure should stay
   in sync with GNATCOLL.OS.Libc.Stat.Statvfs_Info.
 */
struct gnatcoll_statvfs {
    uint_64 bsize;
    uint_64 frsize;
    uint_64 blocks;
    uint_64 bfree;
    uint_64 bavail;
    uint_64 files;
    uint_64 ffree;
    uint_64 favail;
    uint_64 flags;
    uint_64 namemax;
    uint_64 pathmax;
};

int __gnatcoll_statvfs(const char *path, struct gnatcoll_statvfs *buf)
{
   struct statvfs result;
   int status;
   status = statvfs (path, &result);
   buf->bsize = (uint_64) result.f_bsize;
   buf->frsize = (uint_64) result.f_frsize;
   buf->blocks = (uint_64) result.f_blocks;
   buf->bfree = (uint_64) result.f_bfree;
   buf->bavail = (uint_64) result.f_bavail;
   buf->files = (uint_64) result.f_files;
   buf->ffree = (uint_64) result.f_ffree;
   buf->favail = (uint_64) result.f_favail;
   buf->flags = (uint_64) result.f_flag;
   buf->namemax = (uint_64) result.f_namemax;
   if (status == 0) {
      buf->pathmax = (uint_64 ) pathconf (path, _PC_PATH_MAX);
   }
   return status;
}

int __gnatcoll_fstatvfs(int fd, struct gnatcoll_statvfs *buf)
{
   struct statvfs result;
   int status;
   status = fstatvfs (fd, &result);
   buf->bsize = (uint_64) result.f_bsize;
   buf->frsize = (uint_64) result.f_frsize;
   buf->blocks = (uint_64) result.f_blocks;
   buf->bfree = (uint_64) result.f_bfree;
   buf->bavail = (uint_64) result.f_bavail;
   buf->files = (uint_64) result.f_files;
   buf->ffree = (uint_64) result.f_ffree;
   buf->favail = (uint_64) result.f_favail;
   buf->flags = (uint_64) result.f_flag;
   buf->namemax = (uint_64) result.f_namemax;
   if (status == 0) {
      buf->pathmax = (uint_64 ) fpathconf (fd, _PC_PATH_MAX);
   }
   return status;
}

/* Simpler mapping of libc stat struct.

   the representation used is either equal or bigger in size than the
   operating representation.
*/
struct gnatcoll_stat {
    uint_64 dev;     /* ID of device containing file */
    uint_64 ino;     /* inode number */
    uint_32 mode;    /* protection */
    uint_64 nlink;   /* number of hard links*/
    uint_32 uid;     /* user ID of owner */
    uint_32 gid;     /* group ID of owner */
    uint_64 rdev;    /* device ID (if special file) */
    sint_64 size;    /* total size, in bytes */
    sint_64 blksize; /* blocksize for file system I/O */
    sint_64 blocks;  /* number of 512B blocks allocated */
    sint_64 atime;   /* last access time in nano seconds */
    sint_64 mtime;   /* last modification time in nano seconds */
    sint_64 ctime;   /* last metadata change time in nano seconds */
};

int __gnatcoll_stat(const char *path, struct gnatcoll_stat *buf)
{
  struct stat result;
  int status;

  status = stat (path, &result);
  buf->dev = (uint_64) result.st_dev;
  buf->ino = (uint_64) result.st_ino;
  buf->mode = (uint_32) result.st_mode;
  buf->nlink = (uint_64) result.st_nlink;
  buf->uid = (uint_32) result.st_uid;
  buf->gid = (uint_32) result.st_gid;
  buf->rdev = (uint_64) result.st_rdev;
  buf->size = (sint_64) result.st_size;
  buf->blksize = (sint_64) result.st_blksize;
  buf->blocks = (sint_64) result.st_blocks;
#if defined(__APPLE__)
  buf->atime = (sint_64) result.st_atimespec.tv_sec * 1000000000 +
               (sint_64) result.st_atimespec.tv_nsec;
  buf->mtime = (sint_64) result.st_mtimespec.tv_sec * 1000000000 +
               (sint_64) result.st_mtimespec.tv_nsec;
  buf->ctime = (sint_64) result.st_ctimespec.tv_sec * 1000000000 +
               (sint_64) result.st_ctimespec.tv_nsec;
#else
  buf->atime = (sint_64) result.st_atim.tv_sec * 1000000000 +
               (sint_64) result.st_atim.tv_nsec;
  buf->mtime = (sint_64) result.st_mtim.tv_sec * 1000000000 +
               (sint_64) result.st_mtim.tv_nsec;
  buf->ctime = (sint_64) result.st_ctim.tv_sec * 1000000000 +
               (sint_64) result.st_ctim.tv_nsec;

#endif
  return status;
}

int __gnatcoll_lstat(const char *path, struct gnatcoll_stat *buf)
{
  struct stat result;
  int status;

  status = lstat (path, &result);
  buf->dev = (uint_64) result.st_dev;
  buf->ino = (uint_64) result.st_ino;
  buf->mode = (uint_32) result.st_mode;
  buf->nlink = (uint_64) result.st_nlink;
  buf->uid = (uint_32) result.st_uid;
  buf->gid = (uint_32) result.st_gid;
  buf->rdev = (uint_64) result.st_rdev;
  buf->size = (sint_64) result.st_size;
  buf->blksize = (sint_64) result.st_blksize;
  buf->blocks = (sint_64) result.st_blocks;
#if defined(__APPLE__)
  buf->atime = (sint_64) result.st_atimespec.tv_sec * 1000000000 +
               (sint_64) result.st_atimespec.tv_nsec;
  buf->mtime = (sint_64) result.st_mtimespec.tv_sec * 1000000000 +
               (sint_64) result.st_mtimespec.tv_nsec;
  buf->ctime = (sint_64) result.st_ctimespec.tv_sec * 1000000000 +
               (sint_64) result.st_ctimespec.tv_nsec;
#else
  buf->atime = (sint_64) result.st_atim.tv_sec * 1000000000 +
               (sint_64) result.st_atim.tv_nsec;
  buf->mtime = (sint_64) result.st_mtim.tv_sec * 1000000000 +
               (sint_64) result.st_mtim.tv_nsec;
  buf->ctime = (sint_64) result.st_ctim.tv_sec * 1000000000 +
               (sint_64) result.st_ctim.tv_nsec;

#endif
  return status;
}

int __gnatcoll_open(const char *path, int mode, int perm)
{
  return open (path, mode, perm);
}

/* Create a pipe

On Linux system O_CLOEXEC (close on exec) can be set atomically. On other
unix-like systems this should be done by a separate system calls.

*/
int __gnatcoll_pipe(int* fds)
{
#if defined(__linux__) && defined(O_CLOEXEC)
  return pipe2(fds, O_CLOEXEC);
#else
  return pipe(fds);
#endif
}

void *__gnatcoll_posix_spawn_file_actions_init()
{
  posix_spawn_file_actions_t *result = malloc (sizeof (posix_spawn_file_actions_t));
  posix_spawn_file_actions_init(result);
  return (void *) result;
}

void __gnatcoll_posix_spawn_file_actions_destroy(void *file_actions)
{
  posix_spawn_file_actions_destroy((posix_spawn_file_actions_t *) file_actions);
  free (file_actions);
}
