/*----------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2019, AdaCore                          --
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

/* The function is the low level part of GNATCOLL.Utils.Executable_Path. */

#if defined (__APPLE__)
/* MacOS implementation */
#include <stdint.h>
#include <string.h>
#include <mach-o/dyld.h>
int
c_executable_path (char *buffer, int size)
{
    int status;
    uint32_t bufsize = (uint32_t) size;
    status = _NSGetExecutablePath(buffer, &bufsize);
    if (status == 0) {
        return strlen(buffer);
    } else {
        return 0;
    }
}

#elif defined (__MINGW32__) || defined (__CYGWIN__)
/* Windows Implementation */
#include <libloaderapi.h>
int
c_executable_path (char *buffer, int size)
{
    return (int) GetModuleFileNameA(NULL, buffer, (DWORD) size);
}

#elif defined (__linux__)
/* Linux implementation */
#include <unistd.h>
int
c_executable_path (char *buffer, int size)
{
    return readlink("/proc/self/exe", buffer, (size_t) size);
}

#else
/* Dummy implementation */
int
c_executable_path (char *buffer, int size)
{
    return 0;
}
#endif
