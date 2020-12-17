------------------------------------------------------------------------------
--                              G N A T C O L L                             --
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
------------------------------------------------------------------------------

package GNATCOLL.OS.Libc_Constants is

   O_RDONLY   : constant := 16#0000#;
   O_WRONLY   : constant := 16#0000#;
   O_RDWR     : constant := 16#0002#;
   O_NONBLOCK : constant := 16#0004#;
   O_APPEND   : constant := 16#0008#;
   O_CREAT    : constant := 16#0200#;
   O_TRUNC    : constant := 16#0400#;
   O_EXCL     : constant := 16#0800#;
   O_NOCTTY   : constant := 16#20000#;
   O_CLOEXEC  : constant := 16#1000000#;

end GNATCOLL.OS.Libc_Constants;
