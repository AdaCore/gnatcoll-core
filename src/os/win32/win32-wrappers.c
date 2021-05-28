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

#include "winternl.h"

NTSTATUS
__gnatcoll_ntopenfile (HANDLE *handle,
                       PUNICODE_STRING name,
 ACCESS_MASK desired_access,
 PIO_STATUS_BLOCK io,
 ULONG access,
 ULONG options)
{
  OBJECT_ATTRIBUTES attr;
  NTSTATUS status;

  InitializeObjectAttributes(&attr, name, OBJ_CASE_INSENSITIVE, NULL, NULL);
  status = NtOpenFile (handle,
                       desired_access,
                       &attr,
                       io,
                       access,
                       options);
  return status;
}
