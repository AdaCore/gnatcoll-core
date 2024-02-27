with Ada.Unchecked_Deallocation;
with GNATCOLL.OS.Stat;
with GNAT.Strings;

package body GNATCOLL.Buffer is

   package Stat renames GNATCOLL.OS.Stat;

   function Internal_Next
      (Self : in out Reader'Class; C : out Character) return Boolean;

   procedure Free is new Ada.Unchecked_Deallocation
     (String, String_Access);

   -----------
   -- Check --
   -----------

   function Check (Self : in out Reader'Class; Str : String) return Boolean is
      CC : Character;
   begin
      for J in Str'Range loop
         if not Next (Self, CC) or else Str (J) /= CC then
            return False;
         end if;
      end loop;

      return True;
   end Check;

   ------------------
   -- Current_Char --
   ------------------

   function Current_Char (Self : Reader'Class) return Character is
   begin
      return Self.Buffer (Self.Current);
   end Current_Char;

   ----------------------
   -- Current_Position --
   ----------------------

   function Current_Position (Self : Reader'Class) return Long_Long_Integer is
   begin
      return Self.Offset + Long_Long_Integer (Self.Current);
   end Current_Position;

   ---------------------------
   -- Current_Text_Position --
   ---------------------------

   procedure Current_Text_Position
      (Self   : Reader'Class;
       Line   : out Integer;
       Column : out Integer) is
   begin
      Current_Text_Position (Self, Self.Current, Line, Column);
   end Current_Text_Position;

   procedure Current_Text_Position
      (Self   : Reader'Class;
       Offset : Integer;
       Line   : out Integer;
       Column : out Integer)
   is
      Result_Column    : Integer := 0;
      Result_Line      : Integer := 1;
      Effective_Offset : Integer := Offset;
      Prev_Was_LF      : Boolean := False;
   begin

      if not Self.Track_Lines then
         Line := 0;
         Column := 0;
         return;
      end if;

      if Offset > Self.Last then
         Effective_Offset := Self.Last;
      end if;

      for Idx in 1 .. Effective_Offset loop
         Result_Column := Result_Column + 1;
         if Prev_Was_LF then
            Prev_Was_LF := False;
            Result_Line := Result_Line + 1;
            Result_Column := 1;
         end if;

         if Self.Buffer (Idx) = ASCII.LF then
            Prev_Was_LF := True;
         end if;
      end loop;
      Line := Result_Line;
      Column := Result_Column;

   end Current_Text_Position;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Reader) is
      use Mmap;
   begin
      if Self.Auto_Close_FD then
         FS.Close (Self.FD);
      end if;

      if Self.Buffer_Str /= null then
         Free (Self.Buffer_Str);
      end if;

      if Self.File /= Mmap.Invalid_Mapped_File then
         Close (Self.File);
      end if;
   end Finalize;

   -------------------
   -- Internal_Next --
   -------------------

   function Internal_Next
      (Self : in out Reader'Class;
       C    : out Character)
      return Boolean
   is
      Read_Bytes : Integer;
   begin

      --  No character is available in the buffer.
      if Self.FD = FS.Invalid_FD then
         Self.EOF := True;
      end if;

      if Self.EOF then
         --  No more character is available
         C := ASCII.NUL;
         return False;
      end if;

      declare
         --  Compute the total number of chracters that need to be kept in
         --  the window
         Hold : constant Integer := Self.Last - Self.First + 1;
      begin
         if Self.First = 0 or else Hold = 0 then

            --  If no character is hold just fill the buffer
            Read_Bytes := FS.Read (Self.FD, Self.Buffer_Str.all);
            if Read_Bytes = 0 then
               Self.EOF := True;
               C := ASCII.NUL;
               return False;
            end if;

            Self.Offset := Self.Offset + Long_Long_Integer (Self.First) - 1;
            Self.First := 1;
            Self.Last := Read_Bytes;
            Self.Current := 1;
            C := Self.Buffer (Self.Current);

            return True;

         else
            --  Some character are hold.
            if Hold > Self.Buffer_Str'Length / 2 then
               --  Remaining size in the buffer is insufficient for a read
               --  operation. Reallocate the buffer and duplicate its size
               declare
                  New_Buffer : constant String_Access :=
                     new String (1 .. Self.Buffer_Str'Length * 2);
               begin
                  New_Buffer (1 .. Hold) :=
                     Self.Buffer_Str (Self.First .. Self.Last);
                  Free (Self.Buffer_Str);
                  Self.Buffer_Str := New_Buffer;
                  Self.Buffer := Mmap.Short.To_Str_Access
                     (GNAT.Strings.String_Access (Self.Buffer_Str));
               end;
            else
               --  Remaining size in the buffer is sufficient for a read of
               --  half the capacity.
               Self.Buffer_Str (1 .. Hold) :=
                  Self.Buffer_Str (Self.First .. Self.Last);
            end if;

            Self.Offset := Self.Offset + Long_Long_Integer (Self.First) - 1;
            Self.First  := 1;

            Read_Bytes := FS.Read
               (Self.FD, Self.Buffer_Str.all,
                Hold + 1,
                Hold + Self.Buffer_Str'Length / 2);

            if Read_Bytes = 0 then
               Self.EOF := True;
               C := ASCII.NUL;
               return False;
            end if;

            Self.Last := Hold + Read_Bytes;
            Self.Current := Hold + 1;
            C := Self.Buffer (Self.Current);
            return True;
         end if;
      end;

   end Internal_Next;

   --------------------
   -- Is_End_Of_Data --
   --------------------

   function Is_End_Of_Data (Self : Reader'Class) return Boolean is
   begin
      return Self.EOF;
   end Is_End_Of_Data;

   ----------
   -- Next --
   ----------

   function Next
      (Self   : in out Reader'Class;
       C      : out Character;
       Offset : in out Integer)
      return Boolean
   is
   begin
      if Offset < Self.Last then
         Offset := Offset + 1;
         C := Self.Buffer (Offset);
         return True;
      else
         Self.EOF := True;
         C := ASCII.NUL;
         return False;
      end if;
   end Next;

   function Next (Self : in out Reader'Class; C : out Character)
      return Boolean
   is
   begin
      --  Do we have some available characters in the buffer ?
      if Self.Current < Self.Last then
         Self.Current := Self.Current + 1;
         C := Self.Buffer (Self.Current);
         return True;
      else
         return Internal_Next (Self, C);
      end if;
   end Next;

   ----------
   -- Open --
   ----------

   function Open (FD : FS.File_Descriptor) return Reader
   is
      FD_Info    : constant Stat.File_Attributes := Stat.Fstat (FD);
      FD_Size    : constant Long_Long_Integer := Stat.Length (FD_Info);
      Read_Bytes : Integer;
   begin
      return Result : Reader do
         if not Stat.Is_File (FD_Info) or else FD_Size > 64 * 1024
         then
            --  Use stream mode whenever the file is not a regular file or the
            --  file is bigger than 64k. When creating from a buffer we cannot
            --  use mmap.
            Result.Auto_Close_FD := False;
            Result.FD := FD;
            Result.Buffer_Str := new String (1 .. 64 * 1024);
            Result.Buffer := Mmap.Short.To_Str_Access
               (GNAT.Strings.String_Access (Result.Buffer_Str));
            Result.Track_Lines := False;
         elsif FD_Size <= 64 * 1024 then
            --  For small files it's faster to do a regular read. It also
            --  avoids memory fragmentation as mmap always use at least an
            --  entire page.
            Result.Auto_Close_FD := False;
            Result.First := 1;
            Result.Buffer_Str := new String (1 .. Integer (FD_Size));
            Read_Bytes := Read (FD, Result.Buffer_Str.all);
            Result.Last := Read_Bytes;
            Result.Buffer := Mmap.Short.To_Str_Access
               (GNAT.Strings.String_Access (Result.Buffer_Str));
            Result.Track_Lines := False;
         end if;

      end return;
   end Open;

   function Open (Path : UTF8.UTF_8_String) return Reader
   is
      FD_Info    : constant Stat.File_Attributes := Stat.Stat (Path);
      FD_Size    : constant Long_Long_Integer := Stat.Length (FD_Info);
   begin
      return Result : Reader do
         if not Stat.Is_File (FD_Info) or else
            FD_Size > Long_Long_Integer (Integer'Last) or else
            FD_Size > 2 * 1024 * 1024 * 1024
         then
            --  Use stream mode whenever the file is not a regular file or the
            --  file is too big (size > integer'last or size > 2Go)
            Result.FD := FS.Open (Path => Path);
            Result.Buffer_Str := new String (1 .. 64 * 1024);
            Result.Buffer := Mmap.Short.To_Str_Access
               (GNAT.Strings.String_Access (Result.Buffer_Str));
            Result.Track_Lines := False;
         elsif FD_Size <= 64 * 1024 then
            --  For small files it's faster to do a regular read. It also
            --  avoids memory fragmentation as mmap always use at least an
            --  entire page.
            declare
               FD : constant FS.File_Descriptor := FS.Open (Path => Path);
            begin
               Result.First := 1;
               Result.Buffer_Str := new String (1 .. Integer (FD_Size));
               Result.Last := Read (FD, Result.Buffer_Str.all);
               Result.Buffer := Mmap.Short.To_Str_Access
                  (GNAT.Strings.String_Access (Result.Buffer_Str));
               Result.Auto_Close_FD := False;
               FS.Close (FD);
            exception
               when others =>
                  FS.Close (FD);
            end;
         else
            --  In other cases use mmap which is faster
            Result.Auto_Close_FD := False;
            Result.File := Mmap.Open_Read (Path);
            Mmap.Read (Result.File);
            Result.First := 1;
            Result.Last := Integer (FD_Size);
            Result.Buffer := Mmap.Data (Result.File);
         end if;
      end return;
   end Open;

   -----------------
   -- Open_String --
   -----------------

   function Open_String (Str : UTF8.UTF_8_String) return Reader is
   begin
      return Result : Reader do
         Result.Current := 0;
         Result.First := 1;
         Result.Last := Str'Length;
         Result.Buffer_Str := new String (1 .. Str'Length);
         Result.Buffer_Str.all := Str;
         Result.Buffer := Mmap.Short.To_Str_Access
            (GNAT.Strings.String_Access (Result.Buffer_Str));
         Result.EOF := False;
      end return;
   end Open_String;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Reader'Class) is
   begin
      if Self.FD /= FS.Invalid_FD then
         Self.First := Self.Current + 1;
      end if;
   end Release;

   -----------------------
   -- Set_Window_Offset --
   -----------------------

   procedure Set_Window_Offset (Self : in out Reader'Class; Offset : Integer)
   is
   begin
      Self.Current := Offset;
   end Set_Window_Offset;

   -----------
   -- Token --
   -----------

   function Token
      (Self        : Reader'Class;
       First, Last : Long_Long_Integer)
      return String
   is
      Buffer_First : constant Integer := Integer (First - Self.Offset);
      Buffer_Last : constant Integer := Integer (Last - Self.Offset);
   begin
      return Self.Buffer.all (Buffer_First .. Buffer_Last);
   end Token;

   -------------------
   -- Window_Offset --
   -------------------

   function Window_Offset (Self : Reader'Class) return Integer is
   begin
      return Self.Current;
   end Window_Offset;

end GNATCOLL.Buffer;
