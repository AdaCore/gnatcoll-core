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

#include <sys/stat.h>

typedef unsigned long long int uint_64;
typedef unsigned int uint_32;
typedef long long int sint_64;

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
  buf->atime = (sint_64) (result.st_atimespec.tv_sec * 1000000000 +
                          result.st_atimespec.tv_nsec);
  buf->mtime = (sint_64) (result.st_mtimespec.tv_sec * 1000000000 +
                          result.st_mtimespec.tv_nsec);
  buf->ctime = (sint_64) (result.st_ctimespec.tv_sec * 1000000000 +
                          result.st_ctimespec.tv_nsec);
#else
  buf->atime = (sint_64) (result.st_atim.tv_sec * 1000000000 +
                          result.st_atim.tv_nsec);
  buf->mtime = (sint_64) (result.st_mtim.tv_sec * 1000000000 +
                          result.st_mtim.tv_nsec);
  buf->ctime = (sint_64) (result.st_ctim.tv_sec * 1000000000 +
                          result.st_ctim.tv_nsec);

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
  buf->atime = (sint_64) (result.st_atimespec.tv_sec * 1000000000 +
                          result.st_atimespec.tv_nsec);
  buf->mtime = (sint_64) (result.st_mtimespec.tv_sec * 1000000000 +
                          result.st_mtimespec.tv_nsec);
  buf->ctime = (sint_64) (result.st_ctimespec.tv_sec * 1000000000 +
                          result.st_ctimespec.tv_nsec);
#else
  buf->atime = (sint_64) (result.st_atim.tv_sec * 1000000000 +
                          result.st_atim.tv_nsec);
  buf->mtime = (sint_64) (result.st_mtim.tv_sec * 1000000000 +
                          result.st_mtim.tv_nsec);
  buf->ctime = (sint_64) (result.st_ctim.tv_sec * 1000000000 +
                          result.st_ctim.tv_nsec);

#endif
  return status;
}
