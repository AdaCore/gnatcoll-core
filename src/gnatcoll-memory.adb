------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Exceptions.Traceback;
with Ada.Unchecked_Deallocation;
with GNAT.IO;                  use GNAT.IO;
with GNAT.Traceback;           use GNAT.Traceback;
with GNAT.HTable;
with System.Address_Image;
with System.Storage_Elements;  use System.Storage_Elements;

pragma Warnings (Off);
with System.Traceback_Entries; use System.Traceback_Entries;
with System.CRTL;
pragma Warnings (On);

package body GNATCOLL.Memory is

   use Ada.Exceptions;

   Stack_Trace_Depth : Integer := 30;

   Memory_Monitor : Boolean := False;
   Memory_Check   : Boolean := False;

   Disable        : Boolean := False;
   --  This variable is used to avoid infinite loops, where this package would
   --  itself allocate memory and then calls itself recursively, forever.

   type Byte_Count is mod System.Max_Binary_Modulus;

   Total_Allocs   : Byte_Count := 0;
   Alloc_Count    : Long_Integer := 0;
   Total_Free     : Byte_Count := 0;
   Free_Count     : Long_Integer := 0;
   Realloc_Count  : Long_Integer := 0;
   High_Watermark : Byte_Count := 0;

   type Header is range 0 .. 999983 - 1;
   --  Number of elements in the hash-table. This must be relatively big to
   --  avoid slowing down when there are a lot of allocations.

   type Tracebacks_Array_Access
     is access GNAT.Traceback.Tracebacks_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Traceback.Tracebacks_Array, Tracebacks_Array_Access);

   type Traceback_Htable_Elem;
   type Traceback_Htable_Elem_Ptr is access Traceback_Htable_Elem;

   type Traceback_Htable_Elem is record
      Traceback     : Tracebacks_Array_Access;
      Allocs, Frees : Natural;    --  number of allocs and frees
      Total_Allocs  : Byte_Count;
      --  currently allocated memory. This is left to 0 to indicate a special
      --  block created by Mark_Traceback
      Total_Frees   : Byte_Count;
      Next          : Traceback_Htable_Elem_Ptr;
   end record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Traceback_Htable_Elem, Traceback_Htable_Elem_Ptr);

   --  Subprograms used for the Backtrace_Htable instantiation

   procedure Set_Next
     (E    : Traceback_Htable_Elem_Ptr;
      Next : Traceback_Htable_Elem_Ptr);
   function Next
     (E : Traceback_Htable_Elem_Ptr) return Traceback_Htable_Elem_Ptr;
   function Get_Key
     (E : Traceback_Htable_Elem_Ptr) return Tracebacks_Array_Access;
   function Hash (T : Tracebacks_Array_Access) return Header;
   function Equal (K1, K2 : Tracebacks_Array_Access) return Boolean;

   pragma Inline (Set_Next, Next, Get_Key, Hash, Equal);

   package Backtrace_Htable is new GNAT.HTable.Static_HTable
     (Header_Num => Header,
      Element    => Traceback_Htable_Elem,
      Elmt_Ptr   => Traceback_Htable_Elem_Ptr,
      Null_Ptr   => null,
      Set_Next   => Set_Next,
      Next       => Next,
      Key        => Tracebacks_Array_Access,
      Get_Key    => Get_Key,
      Hash       => Hash,
      Equal      => Equal);

   type Chunk_Data is record
      Total           : Byte_Count;
      Alloc_Backtrace : Traceback_Htable_Elem_Ptr;
   end record;
   No_Chunk_Data : constant Chunk_Data := (0, null);

   function Hash (F : System.Address) return Header;
   pragma Inline (Hash);

   package Chunks_Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Header,
      Element    => Chunk_Data,
      No_Element => No_Chunk_Data,
      Key        => System.Address,
      Hash       => Hash,
      Equal      => System."=");

   type Chunk_Kind is (Allocation, Deallocation);
   function Find_Or_Create_Traceback
     (Kind : Chunk_Kind;
      Ptr  : System.Address;
      Size : Storage_Count) return Byte_Count;
   --  Records an allocation or deallocation for the specified Ptr.
   --
   --  Returns the size of the memory that had been allocated for that pointer
   --  in the case of a deallocation, or the size that was just allocated in
   --  the case of an allocation.
   --
   --  To mark a special traceback (Mark_Traceback), we simply set a size of 0,
   --  and a null pointer

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next
     (E    : Traceback_Htable_Elem_Ptr;
      Next : Traceback_Htable_Elem_Ptr)
   is
   begin
      E.Next := Next;
   end Set_Next;

   ----------
   -- Next --
   ----------

   function Next
     (E : Traceback_Htable_Elem_Ptr) return Traceback_Htable_Elem_Ptr is
   begin
      return E.Next;
   end Next;

   -----------
   -- Equal --
   -----------

   function Equal (K1, K2 : Tracebacks_Array_Access) return Boolean is
      use Ada.Exceptions.Traceback;
   begin
      return K1.all = K2.all;
   end Equal;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (E : Traceback_Htable_Elem_Ptr) return Tracebacks_Array_Access
   is
   begin
      return E.Traceback;
   end Get_Key;

   ----------
   -- Hash --
   ----------

   function Hash (T : Tracebacks_Array_Access) return Header is
      Result : Integer_Address := 0;

   begin
      for X in T'Range loop
         Result := Result + To_Integer (PC_For (T (X)));
      end loop;

      return Header (Result mod Integer_Address (Header'Last));
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (F : System.Address) return Header is
   begin
      return Header (1 + To_Integer (F) mod Integer_Address (Header'Last));
   end Hash;

   ------------------------------
   -- Find_Or_Create_Traceback --
   ------------------------------

   function Find_Or_Create_Traceback
     (Kind : Chunk_Kind;
      Ptr  : System.Address;
      Size : Storage_Count) return Byte_Count
   is
      Trace : aliased Tracebacks_Array (1 .. Stack_Trace_Depth);
      Len   : Natural;
      Elem  : Traceback_Htable_Elem_Ptr;
      Chunk : Chunk_Data;
      Size_Was : Byte_Count := 0;
   begin
      if Disable then
         return 0;
      end if;

      Disable := True;

      case Kind is
         when Allocation =>
            Size_Was := Byte_Count (Size);

            Call_Chain (Trace, Len);

            --  Check if the traceback is already in the table
            --  Ignore the first two levels:
            --    'First => Find_Or_Create_Traceback
            --    'First + 1 => malloc or free

            Elem :=
              Backtrace_Htable.Get
                (Trace (Trace'First + 2 .. Len)'Unrestricted_Access);

            --  If not, insert it

            if Elem = null then
               Elem := new Traceback_Htable_Elem'
                 (Traceback =>
                  new Tracebacks_Array'(Trace (Trace'First + 2 .. Len)),
                  Allocs        => 1,
                  Frees         => 0,
                  Total_Allocs  => Size_Was,
                  Total_Frees   => 0,
                  Next          => null);
               Backtrace_Htable.Set (Elem);
            else
               Elem.Allocs       := Elem.Allocs + 1;
               Elem.Total_Allocs := Elem.Total_Allocs + Size_Was;
            end if;

            if Ptr /= System.Null_Address then
               Chunks_Htable.Set
                 (Ptr, (Total => Size_Was, Alloc_Backtrace => Elem));
            end if;

         when Deallocation =>
            if Ptr /= System.Null_Address then
               Chunk              := Chunks_Htable.Get (Ptr);

               --  Protect ourselves in case the allocation was done in C for
               --  instance (which should not happen, of course)
               if Chunk /= No_Chunk_Data then
                  Size_Was           := Chunk.Total;
                  Elem               := Chunk.Alloc_Backtrace;
                  Elem.Frees         := Elem.Frees + 1;
                  Elem.Total_Frees   := Elem.Total_Frees + Size_Was;
                  Chunks_Htable.Remove (Ptr);
               end if;
            end if;

            --  Never remove the chunk info, since we might be allocating again
            --  at the same location later on, and in any case it might count
            --  in the biggest number of allocs for instance.
      end case;

      Disable := False;
      return Size_Was;
   end Find_Or_Create_Traceback;

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return System.Address is
      Result      : System.Address;
      Actual_Size : size_t := Size;

      Size_Was    : Byte_Count;

   begin
      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      --  Change size from zero to non-zero. We still want a proper pointer
      --  for the zero case because pointers to zero length objects have to
      --  be distinct, but we can't just go ahead and allocate zero bytes,
      --  since some malloc's return zero for a zero argument.

      if Size = 0 then
         Actual_Size := 1;
      end if;

      Result := System.CRTL.malloc (System.CRTL.size_t (Actual_Size));

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      if Memory_Monitor then
         Size_Was := Find_Or_Create_Traceback
           (Allocation, Result, Storage_Count (Actual_Size));

         Total_Allocs := Total_Allocs + Size_Was;
         High_Watermark := Byte_Count'Max
           (High_Watermark, Total_Allocs - Total_Free);
         Alloc_Count := Alloc_Count + 1;
      end if;

      return Result;
   end Alloc;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : System.Address) is
      Size_Was    : Byte_Count;
   begin
      if not Memory_Check then
         System.CRTL.free (Ptr);
      end if;

      if Memory_Monitor then
         Size_Was   := Find_Or_Create_Traceback (Deallocation, Ptr, 0);
         Total_Free := Total_Free + Size_Was;
         Free_Count := Free_Count + 1;
      end if;
   end Free;

   -------------
   -- Realloc --
   -------------

   function Realloc
     (Ptr  : System.Address;
      Size : size_t)
      return System.Address
   is
      Result      : System.Address;
      Actual_Size : constant size_t := Size;
      Size_Was    : Byte_Count;
   begin
      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      Result := System.CRTL.realloc (Ptr, System.CRTL.size_t (Actual_Size));

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      if Memory_Monitor then
         Size_Was   := Find_Or_Create_Traceback (Deallocation, Ptr, 0);
         Total_Free := Total_Free + Size_Was;

         Size_Was := Find_Or_Create_Traceback
           (Allocation, Result, Storage_Count (Actual_Size));

         Total_Allocs := Total_Allocs + Size_Was;
         High_Watermark := Byte_Count'Max
           (High_Watermark, Total_Allocs - Total_Free);
         Realloc_Count := Realloc_Count + 1;
      end if;

      return Result;
   end Realloc;

   ----------
   -- Dump --
   ----------

   procedure Dump (Size : Positive; Report : Report_Type := All_Reports) is
      procedure Do_Report (Sort : Report_Type);
      --  Do a specific type of report

      procedure Do_Report (Sort : Report_Type) is
         Elem        : Traceback_Htable_Elem_Ptr;
         Bigger      : Boolean;
         Grand_Total : Float;

         Max  : array (1 .. Size) of Traceback_Htable_Elem_Ptr :=
           (others => null);
         --  Sorted array for the biggest memory users
      begin
         New_Line;
         case Sort is
            when Memory_Usage | All_Reports  =>
               Put_Line (Size'Img & " biggest memory users at this time:");
               Put_Line ("Results include bytes and chunks still allocated");
               Grand_Total := Float (Total_Allocs - Total_Free);
            when Allocations_Count =>
               Put_Line (Size'Img & " biggest number of live allocations:");
               Put_Line ("Results include bytes and chunks still allocated");
               Grand_Total := Float (Total_Allocs - Total_Free);
            when Sort_Total_Allocs =>
               Put_Line (Size'Img & " biggest number of allocations:");
               Put_Line ("Results include total bytes and chunks allocated,");
               Put_Line ("even if no longer allocated - Deallocations are"
                         & " ignored");
               Grand_Total := Float (Total_Allocs);
            when Marked_Blocks =>
               Put_Line ("Special blocks marked by Mark_Traceback");
               Grand_Total := 0.0;
         end case;

         Elem := Backtrace_Htable.Get_First;
         while Elem /= null loop
            --  Ignore small blocks (depending on the sorting criteria) to gain
            --  speed

            if (Sort = Memory_Usage
                and then Elem.Total_Allocs - Elem.Total_Frees >= 1_000)
              or else (Sort = Allocations_Count
                       and then Elem.Allocs - Elem.Frees > 1)
              or else (Sort = Sort_Total_Allocs and then Elem.Allocs > 1)
              or else (Sort = Marked_Blocks
                       and then Elem.Total_Allocs = 0)
            then
               if Sort = Marked_Blocks then
                  Grand_Total := Grand_Total + Float (Elem.Allocs);
               end if;

               for M in Max'Range loop
                  Bigger := Max (M) = null;
                  if not Bigger then
                     case Sort is
                        when Memory_Usage | All_Reports =>
                           Bigger :=
                             Max (M).Total_Allocs - Max (M).Total_Frees <
                             Elem.Total_Allocs - Elem.Total_Frees;
                        when Allocations_Count =>
                           Bigger :=
                             Max (M).Allocs - Max (M).Frees
                             < Elem.Allocs - Elem.Frees;
                        when Sort_Total_Allocs | Marked_Blocks =>
                           Bigger := Max (M).Allocs < Elem.Allocs;
                     end case;
                  end if;

                  if Bigger then
                     Max (M + 1 .. Max'Last) := Max (M .. Max'Last - 1);
                     Max (M) := Elem;
                     exit;
                  end if;
               end loop;
            end if;
            Elem := Backtrace_Htable.Get_Next;
         end loop;

         if Grand_Total = 0.0 then
            Grand_Total := 1.0;
         end if;

         for M in Max'Range loop
            exit when Max (M) = null;
            declare
               type Percent is delta 0.1 range 0.0 .. 100.0;
               Total : Byte_Count;
               P : Percent;
            begin
               case Sort is
                  when Memory_Usage | Allocations_Count | All_Reports =>
                     Total := Max (M).Total_Allocs - Max (M).Total_Frees;
                  when Sort_Total_Allocs =>
                     Total := Max (M).Total_Allocs;
                  when Marked_Blocks =>
                     Total := Byte_Count (Max (M).Allocs);
               end case;

               P := Percent (100.0 * Float (Total) / Grand_Total);

               if Sort = Marked_Blocks then
                  Put (P'Img & "%:"
                       & Max (M).Allocs'Img & " chunks /"
                       & Integer (Grand_Total)'Img & " at");
               else
                  Put (P'Img & "%:" & Total'Img & " bytes in"
                       & Max (M).Allocs'Img & " chunks at");
               end if;
            end;

            for J in Max (M).Traceback'Range loop
               Put (" 0x" & Address_Image (PC_For (Max (M).Traceback (J))));
            end loop;

            New_Line;
         end loop;
      end Do_Report;

   begin
      if Disable then
         return;
      end if;

      if not Memory_Monitor then
         Put_Line ("Memory monitor not activated");
         return;
      end if;

      Disable := True;

      Put_Line ("Ada Allocs:" & Total_Allocs'Img
                & " bytes in" & Alloc_Count'Img & " chunks");
      Put_Line ("Ada Free:" & Total_Free'Img & " bytes in" & Free_Count'Img
                & " chunks");
      Put_Line ("Ada Realloc: " & Realloc_Count'Img & " calls");
      Put_Line ("Ada Current watermark: "
                & Byte_Count'Image (Total_Allocs - Total_Free)
                & " in" & Long_Integer'Image (Alloc_Count - Free_Count)
                & " chunks");
      Put_Line ("Ada High watermark: " & High_Watermark'Img);

      case Report is
         when All_Reports =>
            for Sort in Report_Type loop
               if Sort /= All_Reports then
                  Do_Report (Sort);
               end if;
            end loop;

         when others =>
            Do_Report (Report);
      end case;

      Disable := False;
   end Dump;

   -----------
   -- Reset --
   -----------

   procedure Reset is
      Elem, Tmp : Traceback_Htable_Elem_Ptr;
   begin
      if Disable then
         return;
      end if;

      Disable := True;

      Chunks_Htable.Reset;

      Elem := Backtrace_Htable.Get_First;
      while Elem /= null loop
         Tmp := Elem;
         Elem := Backtrace_Htable.Get_Next;
         Unchecked_Free (Tmp.Traceback);
         Unchecked_Free (Tmp);
      end loop;

      Backtrace_Htable.Reset;

      Total_Allocs   := 0;
      Alloc_Count    := 0;
      Total_Free     := 0;
      Free_Count     := 0;
      Realloc_Count  := 0;
      High_Watermark := 0;

      Disable := False;
   end Reset;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Activate_Monitor  : Boolean := False;
      Disable_Free      : Boolean := False;
      Stack_Trace_Depth : Positive := 30) is
   begin
      Memory_Monitor := Activate_Monitor;
      Memory_Check   := Disable_Free;
      GNATCOLL.Memory.Stack_Trace_Depth := Stack_Trace_Depth;
   end Configure;

   --------------------
   -- Mark_Traceback --
   --------------------

   procedure Mark_Traceback is
      Size_Was : Byte_Count;
      pragma Unreferenced (Size_Was);
   begin
      if Memory_Monitor then
         Size_Was := Find_Or_Create_Traceback
           (Kind => Allocation,
            Ptr  => System.Null_Address,
            Size => 0);
      end if;
   end Mark_Traceback;

end GNATCOLL.Memory;
