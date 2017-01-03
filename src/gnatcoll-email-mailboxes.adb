------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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

with Ada.Calendar;          use Ada.Calendar;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.IO_Exceptions;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Boyer_Moore;  use GNATCOLL.Boyer_Moore;
with GNATCOLL.Email.Parser; use GNATCOLL.Email.Parser;
with GNATCOLL.Email.Utils;  use GNATCOLL.Email.Utils;
with GNATCOLL.Mmap;         use GNATCOLL.Mmap;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with GNAT.Strings;          use GNAT.Strings;

package body GNATCOLL.Email.Mailboxes is
   use Message_Info_List, Cursor_List;

   From_Pattern : GNATCOLL.Boyer_Moore.Pattern;
   --  An efficient search pattern for the From_ lines that separate messages
   --  in a mbox

   procedure Internal_Search_Start
     (Self   : Mbox'Class;
      Buffer : String;
      From   : in out Integer);
   --  Search the start of the next message after the position From.
   --  It should return -1 if no further message exists.
   --  This is for the implementation of new mailbox types only, and is not
   --  needed when using standard mailboxes.

   function Earlier_Than
     (Left, Right : Abstract_Message_Info'Class) return Boolean;
   --  Compare two elements by date

   type Container;
   type Container_Access is access Container;
   type Container is record
      Msg    : Message;
      Parent : Container_Access;
      Child  : Container_Access;
      Next   : Container_Access;
   end record;
   package Container_Hash is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Container_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   use Container_Hash;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Container, Container_Access);

   ------------------
   -- Earlier_Than --
   ------------------

   function Earlier_Than
     (Left, Right : Abstract_Message_Info'Class) return Boolean
   is
      Date1 : constant Time := Get_Date (Left.Msg);
      Date2 : constant Time := Get_Date (Right.Msg);
   begin
      return Date1 < Date2;
   end Earlier_Than;

   package Pkg_Sort_By_Date is new Message_Info_List.Generic_Sorting
     ("<" => Earlier_Than);

   -----------------
   -- Free_String --
   -----------------

   procedure Free_String (Str : in out GNAT.Strings.String_Access) is
   begin
      GNAT.Strings.Free (Str);
   end Free_String;

   ----------------
   -- Set_Parser --
   ----------------

   procedure Set_Parser
     (Self     : in out Cursor;
      Factory  : Message_Factory := Email.Parser.Parse'Access)
   is
   begin
      Self.Factory := Factory;
   end Set_Parser;

   ----------
   -- Open --
   ----------

   procedure Open
     (Self     : in out Mbox;
      Fp       : access String;
      On_Close : Destructor      := Free_String'Access) is
   begin
      Self.Fp       := GNAT.Strings.String_Access (Fp);
      Self.On_Close := On_Close;
   end Open;

   ----------
   -- Open --
   ----------

   procedure Open
     (Self     : in out Mbox;
      Filename : Virtual_File) is
   begin
      Self.Fp         := Read_File (Filename);
      Self.On_Close   := Free_String'Access;
      if Self.Fp = null then
         raise Ada.IO_Exceptions.Name_Error with Filename.Display_Full_Name;
      end if;
   end Open;

   -----------
   -- First --
   -----------

   function First (Self : Mbox) return Cursor'Class is
   begin
      declare
         Cur : Cursor'Class := Mbox_Cursor'
           (Cursor with
            Max     => Self.Fp'Last,
            Start   => Self.Fp'First,
            Stop    => 0,
            Current => Null_Message);
      begin
         Next (Cur, Self);
         return Cur;
      end;
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : Mbox_Cursor) return Boolean is
   begin
      return Self.Stop <= Self.Max;
   end Has_Element;

   -----------------
   -- Get_Message --
   -----------------

   procedure Get_Message
     (Self : in out Mbox_Cursor;
      Box  : Mailbox'Class;
      Msg  : out Message)
   is
      Buffer : Str_Access;
   begin
      --  Already cached ?

      if Self.Current /= Null_Message then
         Msg := Self.Current;

      --  Already past the end ?

      elsif Self.Stop > Self.Max
        or else Self.Factory = null
      then
         Msg := Null_Message;

      else
         if Mbox (Box).Fp /= null then
            Buffer := To_Str_Access (Mbox (Box).Fp);
         end if;

         Self.Factory
           (String (Buffer (Self.Start .. Self.Stop)), Self.Current);
         Msg := Self.Current;
      end if;
   end Get_Message;

   ----------
   -- Next --
   ----------

   procedure Next
     (Self : in out Mbox_Cursor;
      Box  : Mailbox'Class)
   is
      Buffer : Str_Access;
      First  : Integer;

      Skip_Separating_Newline : constant Integer := 2;
      --  Number of characters to move backward from a "From_" substring to
      --  find the end of the previous message. This includes skipping over the
      --  newline character that must separate messages.

   begin
      Self.Current := Null_Message;

      --  Already past the end ?

      if Self.Stop >= Self.Max then
         Self.Stop := Self.Max + 1;
         return;
      end if;

      if Mbox (Box).Fp /= null then
         Buffer := To_Str_Access (Mbox (Box).Fp);
         First := Mbox (Box).Fp'First;
      else
         return;  --  Nothing to parse
      end if;

      --  Find start of first message if needed. If we are past this first
      --  message, we know we left the pointer to the start of a message, no
      --  need to check again.

      if Self.Stop = 0 then
         if String (Buffer (First .. 5)) /= "From " then
            Self.Stop := First;
            Internal_Search_Start
              (Mbox'Class (Box),
               String (Buffer (First .. Self.Max)),
               Self.Stop);
            if Self.Stop = -1 then
               Self.Stop    := Integer'Last;
               Self.Current := Null_Message;
               return;
            else
               Self.Stop := Self.Stop - Skip_Separating_Newline;
            end if;
         else
            Self.Stop := First - Skip_Separating_Newline;
         end if;
      end if;

      --  At this point Self.Stop points to the first character before the
      --  beginning of the next message

      if Self.Stop >= Self.Max then
         return;
      end if;

      Self.Start := Self.Stop + Skip_Separating_Newline;

      --  Search end of message
      --  Skip current From_ line
      Self.Stop := Self.Start + 5;   --  "From_"'Length
      Internal_Search_Start
        (Mbox'Class (Box),
         String (Buffer (Self.Stop .. Self.Max)),
         Self.Stop);

      if Self.Stop < 0 then
         --  No message after this one => it extends till the end of the buffer
         Self.Stop := Self.Max;
      else
         Self.Stop := Self.Stop - Skip_Separating_Newline;
      end if;
   end Next;

   ---------------------------
   -- Internal_Search_Start --
   ---------------------------

   procedure Internal_Search_Start
     (Self   : Mbox'Class;
      Buffer : String;
      From   : in out Integer)
   is
      pragma Unreferenced (Self);
   begin
      --  Note: we used to treat a From_ line that does not contain a colon
      --  as part of the message body. This was not the intent of the code,
      --  and in any case was a questionable heuristic, since all From_ lines
      --  in bodies should really be assumed to have been escaped as >From_.
      --  So, we now just search for From_ at beginning of line.
      --  put in place a heuristic

      From := Search (From_Pattern, Buffer (From .. Buffer'Last));
      if From /= -1 then

         --  Match present: skip initial ASCII.LF

         From := From + 1;
      end if;
   end Internal_Search_Start;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Mailbox) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Mbox) is
   begin
      if Self.On_Close /= null and then Self.Fp /= null then
         Self.On_Close (Self.Fp);
         Self.Fp := null;
      end if;
   end Finalize;

   -----------
   -- Store --
   -----------

   procedure Store
     (Self    : out Stored_Mailbox;
      Box     : in out Mailbox'Class;
      Factory : Message_Factory := Email.Parser.Parse'Access)
   is
   begin
      Store (Self, Box, Factory, First (Box));
   end Store;

   -----------
   -- Store --
   -----------

   procedure Store
     (Self    : out Stored_Mailbox;
      Box     : in out Mailbox'Class;
      Factory : Message_Factory := Email.Parser.Parse'Access;
      From    : Cursor'Class)
   is
      Msg : Message;
      Curs : Cursor'Class := From;
   begin
      Set_Parser (Curs, Factory);
      while Has_Element (Curs) loop
         Get_Message (Curs, Box, Msg);
         if Msg /= Null_Message then
            Append (Self, Msg);
         end if;

         Next (Curs, Box);
      end loop;
   end Store;

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Stored_Mailbox; Msg : Message) is
   begin
      Append
        (Self.Messages,
         Message_Info'(Msg => Msg, Children => Message_Info_List.Empty_List));
      Self.Sorted_By := Sort_None;
   end Append;

   -----------
   -- First --
   -----------

   function First (Self : Stored_Mailbox) return Cursor'Class is
   begin
      return First (Self, Recurse => False);
   end First;

   -----------
   -- First --
   -----------

   function First
     (Self : Stored_Mailbox; Recurse : Boolean)
      return Stored_Mailbox_Cursor'Class
   is
      L : Stored_Mailbox_Cursor;
   begin
      L.Recurse := Recurse;
      L.Thread_Level := 1;
      if not Is_Empty (Self.Messages) then
         Append (L.Cursors, First (Self.Messages));
      end if;
      return L;
   end First;

   ---------------------
   -- First_In_Thread --
   ---------------------

   function First_In_Thread
     (Self : Stored_Mailbox; Parent : Stored_Mailbox_Cursor'Class)
      return Stored_Mailbox_Cursor'Class
   is
      L : Stored_Mailbox_Cursor;

      procedure Get_First (M : Abstract_Message_Info'Class);
      procedure Get_First (M : Abstract_Message_Info'Class) is
      begin
         if Has_Element (First (Message_Info (M).Children)) then
            Append (L.Cursors, First (Message_Info (M).Children));
         else
            L.Cursors := Cursor_List.Empty_List;
         end if;
      end Get_First;

   begin
      if Self.Threaded
        and then not Is_Empty (Parent.Cursors)
        and then Has_Element (Element (Last (Parent.Cursors)))
      then
         L.Recurse := False;
         L.Thread_Level := Parent.Thread_Level + 1;
         Query_Element
           (Element (Last (Parent.Cursors)),
            Get_First'Unrestricted_Access);
         return L;
      else
         L.Cursors := Cursor_List.Empty_List;
         return L;
      end if;
   end First_In_Thread;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Stored_Mailbox_Cursor) return Boolean
   is
   begin
      return not Is_Empty (Self.Cursors);
   end Has_Element;

   -----------------
   -- Get_Message --
   -----------------

   procedure Get_Message
     (Self : in out Stored_Mailbox_Cursor;
      Box  : Mailbox'Class;
      Msg  : out Message)
   is
      pragma Unreferenced (Box);
      Saved : Message_Info_List.Cursor;
   begin
      if Is_Empty (Self.Cursors) then
         Msg := Null_Message;
      else
         Saved := Element (Last (Self.Cursors));
         Msg := Message_Info (Element (Saved)).Msg;
      end if;
   end Get_Message;

   ----------------------
   -- Get_Thread_Level --
   ----------------------

   function Get_Thread_Level (Iter : Stored_Mailbox_Cursor) return Positive is
   begin
      if Is_Empty (Iter.Cursors) then
         return 1;
      else
         return Iter.Thread_Level;
      end if;
   end Get_Thread_Level;

   ----------
   -- Next --
   ----------

   procedure Next
     (Self : in out Stored_Mailbox_Cursor;
      Box  : Mailbox'Class)
   is
      pragma Unreferenced (Box);

      M     : Message_Info;
      Saved : Message_Info_List.Cursor;

      procedure Move_To_Next (C : in out Message_Info_List.Cursor);

      procedure Move_To_Next (C : in out Message_Info_List.Cursor) is
      begin
         Next (C);
      end Move_To_Next;

   begin
      if not Is_Empty (Self.Cursors) then
         Saved := Element (Last (Self.Cursors));
         M     := Message_Info (Element (Saved));

         --  If we have children for the current element, return these next
         if Self.Recurse and then not Is_Empty (M.Children) then
            declare
               procedure Get_First (M : Abstract_Message_Info'Class);
               procedure Get_First (M : Abstract_Message_Info'Class) is
               begin
                  Append (Self.Cursors, First (Message_Info (M).Children));
               end Get_First;
            begin
               Self.Thread_Level := Self.Thread_Level + 1;
               Query_Element (Saved, Get_First'Unrestricted_Access);
            end;

         else
            --  Otherwise move to the next element at the same level if there's
            --  one. Or move to the next element at the parent level,
            --  recursively
            Update_Element
              (Self.Cursors, Last (Self.Cursors),
               Move_To_Next'Unrestricted_Access);

            if Self.Recurse then
               while not Has_Element (Element (Last (Self.Cursors))) loop
                  --  Else move to the next element at the parent level,
                  --  recursively
                  Delete_Last (Self.Cursors);
                  exit when Is_Empty (Self.Cursors);
                  Self.Thread_Level := Self.Thread_Level - 1;
                  Update_Element
                    (Self.Cursors, Last (Self.Cursors),
                     Move_To_Next'Unrestricted_Access);
               end loop;

            elsif not Has_Element (Element (Last (Self.Cursors))) then
               Self.Cursors := Cursor_List.Empty_List;
            end if;
         end if;
      end if;
   end Next;

   --------------------
   -- Remove_Threads --
   --------------------

   procedure Remove_Threads (Stored : in out Stored_Mailbox) is
   begin
      if Stored.Threaded then
         declare
            Iter  : Stored_Mailbox_Cursor'Class :=
              First (Stored, Recurse => True);
            Tmp   : Message_Info_List.List;
            Msg   : Message;
         begin
            while Has_Element (Iter) loop
               Get_Message (Iter, Stored, Msg);
               if Msg /= Null_Message then
                  Append
                    (Tmp,
                     Message_Info'(Msg => Msg,
                                   Children => Message_Info_List.Empty_List));
               end if;
               Next (Iter, Stored);
            end loop;
            Move (Target => Stored.Messages, Source => Tmp);
            Stored.Threaded  := False;
            Stored.Sorted_By := Sort_None;
         end;
      end if;
   end Remove_Threads;

   ------------------
   -- Sort_By_Date --
   ------------------

   procedure Sort_By_Date (Self : in out Stored_Mailbox) is
      procedure Sort_Level (List : in out Message_Info_List.List);
      procedure Sort_Info (Info : in out Abstract_Message_Info'Class);
      --  Sort List, and all children

      procedure Sort_Info (Info : in out Abstract_Message_Info'Class) is
      begin
         Sort_Level (Message_Info (Info).Children);
      end Sort_Info;

      procedure Sort_Level (List : in out Message_Info_List.List) is
         C : Message_Info_List.Cursor;
      begin
         Pkg_Sort_By_Date.Sort (List);
         C := First (List);
         while Has_Element (C) loop
            if not Is_Empty (Message_Info (Element (C)).Children) then
               Update_Element (List, C, Sort_Info'Unrestricted_Access);
            end if;

            Next (C);
         end loop;
      end Sort_Level;
   begin
      if Self.Sorted_By /= Sort_Date then
         if Self.Threaded then
            Sort_Level (Self.Messages);
         else
            Pkg_Sort_By_Date.Sort (Self.Messages);
         end if;

         Self.Sorted_By := Sort_Date;
      end if;
   end Sort_By_Date;

   ---------------------
   -- Thread_Messages --
   ---------------------

   procedure Thread_Messages (Self : in out Stored_Mailbox) is
      Ids : Container_Hash.Map;

      procedure Store_Parent_Of
        (Parent_Cont : in out Container_Access; Id : String);
      --  Memorize that Parent_Cont is the parent of the message whose
      --  Message-ID is Id.
      --  On exit, Parent_Cont is set to the container used for the message
      --  itself.

      procedure Set_Parent_For_All_Ids
        (Parent_Cont : in out Container_Access;
         H           : Header);
      --  For each message-id found in H, call Store_Parent_Of. This
      --  sets a list of dependencies between mail messages.

      procedure Set_Parent_Of
        (Cont        : Container_Access;
         Parent_Cont : Container_Access;
         Override    : Boolean := False);
      --  Set the parent of Cont to be Parent_Cont.
      --  If Cont already has a parent and Override is True, it is replaced.
      --  This is only needed when processing the last item in the References:
      --  field, which we know is the real parent. Previous parents might have
      --  been set while processing References: fields in other messages.
      --  This also update child links

      procedure Put_Threads_In_Message
        (Parent     : in out Message_Info_List.List;
         Root       : Container_Access;
         Root_Level : Boolean);
      --  Put Root in Parent, and all its children recursively

      function Is_Reachable (A, B : Container_Access) return Boolean;
      --  Return True if B is part of the descendant of A. This doesn't test
      --  that A /= B.

      ------------------
      -- Is_Reachable --
      ------------------

      function Is_Reachable (A, B : Container_Access) return Boolean is
         C : Container_Access := A.Child;
         C2 : Container_Access;
      begin
         if A = B then
            return True;
         end if;

         while C /= null loop
            if C = B then
               return True;
            end if;

            C2 := C.Child;

            while C2 /= null loop
               if Is_Reachable (C2, B) then
                  return True;
               end if;

               C2 := C2.Next;
            end loop;

            C := C.Next;
         end loop;
         return False;
      end Is_Reachable;

      -------------------
      -- Set_Parent_Of --
      -------------------

      procedure Set_Parent_Of
        (Cont        : Container_Access;
         Parent_Cont : Container_Access;
         Override    : Boolean := False)
      is
         C : Container_Access;
      begin
         if Parent_Cont = null or else Parent_Cont = Cont then
            return;
         end if;

         if Cont.Parent /= null and then not Override then
            return;
         end if;

         --  Check we do not introduce a loop A -> B -> A. This must be done
         --  before removing the old parent

         if Is_Reachable (Cont, Parent_Cont)
           or else Is_Reachable (Parent_Cont, Cont)
         then
            return;
         end if;

         --  Set the new parent (remove the old one, since if we reach here
         --  with an old parent, we are in Override mode)

         if Cont.Parent /= null then
            if Cont.Parent.Child = Cont then
               Cont.Parent.Child := Cont.Next;
            else
               C := Cont.Parent.Child;
               while C.Next /= null loop
                  if C.Next = Cont then
                     C.Next := Cont.Next;
                     exit;
                  end if;
                  C := C.Next;
               end loop;
            end if;
            Cont.Parent := null;
            Cont.Next := null;
         end if;

         Cont.Parent := Parent_Cont;
         if Parent_Cont /= null then
            if Parent_Cont.Child = null then
               Parent_Cont.Child := Cont;
            else
               C := Parent_Cont.Child;
               while C.Next /= null loop
                  C := C.Next;
               end loop;
               C.Next := Cont;
            end if;
         end if;

         Cont.Next := null;
      end Set_Parent_Of;

      ---------------------
      -- Store_Parent_Of --
      ---------------------

      procedure Store_Parent_Of
        (Parent_Cont : in out Container_Access; Id : String)
      is
         Id_C  : Container_Hash.Cursor;
         Cont  : Container_Access;
      begin
         Id_C := Find (Ids, Id);
         if Has_Element (Id_C) then
            Cont := Element (Id_C);
         else
            Cont := new Container;  --  Null_Message;
            Insert (Ids, Id, Cont);
         end if;

         if Parent_Cont /= null
           and then Cont.Parent = null
         then
            Set_Parent_Of (Cont => Cont, Parent_Cont => Parent_Cont);
         end if;

         Parent_Cont := Cont;
      end Store_Parent_Of;

      ----------------------------
      -- Set_Parent_For_All_Ids --
      ----------------------------

      procedure Set_Parent_For_All_Ids
        (Parent_Cont : in out Container_Access;
         H           : Header)
      is
         Flat  : Unbounded_String;
         Index : Natural;
         Stop  : Natural;
      begin
         if H /= Null_Header then
            Flatten (Get_Value (H), Flat);

            declare
               StrA : constant String := To_String (Flat);
            begin
               Index := StrA'First;
               while Index <= StrA'Last loop
                  Index := Next_Occurrence (StrA (Index .. StrA'Last), '<');
                  Stop := Next_Occurrence (StrA (Index + 1 .. StrA'Last), '>');
                  Store_Parent_Of (Parent_Cont => Parent_Cont,
                                   Id => StrA (Index + 1 .. Stop - 1));
                  Index := Stop + 1;
               end loop;
            end;
         end if;
      end Set_Parent_For_All_Ids;

      ----------------------------
      -- Put_Threads_In_Message --
      ----------------------------

      procedure Put_Threads_In_Message
        (Parent     : in out Message_Info_List.List;
         Root       : Container_Access;
         Root_Level : Boolean)
      is
         C : Container_Access;
         M : Message_Info;
      begin
         --  Do not insert dummy containers

         if Root.Msg = Null_Message then
            if Root.Child = null then
               return;
            end if;

            --  Promote the children one level up

            C := Root.Child;
            while C /= null loop
               Put_Threads_In_Message (Parent, C, Root_Level);
               C := C.Next;
            end loop;

            return;
         else
            M.Msg := Root.Msg;
            Append (Parent, M);
            C := Root.Child;
         end if;

         while C /= null loop
            declare
               procedure Add_Child (MI : in out Abstract_Message_Info'Class);
               procedure Add_Child (MI : in out Abstract_Message_Info'Class) is
               begin
                  Put_Threads_In_Message
                    (Message_Info (MI).Children, C, False);
               end Add_Child;
            begin
               Update_Element
                 (Parent, Last (Parent), Add_Child'Unrestricted_Access);
            end;
            C := C.Next;
         end loop;
      end Put_Threads_In_Message;

      C           : Message_Info_List.Cursor;
      Msg         : Message;
      Msg_Cont    : Container_Access;
      Parent_Cont : Container_Access;
      Id_C        : Container_Hash.Cursor;

      Dummy_Id : Natural := 0;
      --  For those messages that have no Message-Id: field, so that we don't
      --  lose any...

   begin
      --  See algorithm at http://www.jwz.org/doc/threading.html

      if not Self.Threaded then
         C := First (Self.Messages);
         while Has_Element (C) loop
            Msg := Element (C).Msg;

            --  Store a new container for this message-id in the table
            declare
               Id : constant String := Get_Message_Id (Msg);
            begin
               Msg_Cont := null;

               Store_Parent_Of (Parent_Cont => Msg_Cont,
                                Id          => Id);

               --  If there was already such a message-id (in theory not
               --  possible, in practice we might have messages with no
               --  Message-Id: header, and we still want to keep all messages.
               --  We create a dummy, unique, invalid ID containing a space.

               while Msg_Cont.Msg /= Null_Message loop
                  Msg_Cont := null;
                  Store_Parent_Of
                    (Parent_Cont => Msg_Cont,
                     Id          => Id & Integer'Image (Dummy_Id));
                  Dummy_Id := Dummy_Id + 1;
               end loop;
            end;
            Msg_Cont.Msg := Element (C).Msg;

            --  For each of the References: id:

            Parent_Cont := null;
            Set_Parent_For_All_Ids
              (Parent_Cont => Parent_Cont,
               H           => Get_Header (Msg, "References"));

            --  Take into account the In-Reply-To field. In most cases, this
            --  should duplicate the last element of References:, but some
            --  mailers do not behave correctly, so it doesn't harm to
            --  do the work again

            Set_Parent_For_All_Ids
              (Parent_Cont => Parent_Cont,
               H           => Get_Header (Msg, "In-Reply-To"));

            Set_Parent_Of
              (Msg_Cont, Parent_Cont => Parent_Cont, Override => True);

            Next (C);
         end loop;

         --  Prune empty containers. These come from invalid references in
         --  the headers
         --  ??? Nothing do do in fact, we'll just ignore these when putting
         --  messages back in Self.Messages.
         --  We would also promote the child of empty containers with one child
         --  and no parent to the root set.

         --  Find all elements of the root set
         --  ??? Not needed, we do it when we put messages back in
         --  Self.Messages

         --  Sort messages by Subject, to find additional threads
         --  ??? Disabled for now

         Clear (Self.Messages);

         Id_C := First (Ids);
         while Has_Element (Id_C) loop
            if Element (Id_C).Parent = null then
               Put_Threads_In_Message
                 (Parent     => Self.Messages,
                  Root       => Element (Id_C),
                  Root_Level => True);
            end if;

            Next (Id_C);
         end loop;

         --  Free memory
         Id_C := First (Ids);
         while Has_Element (Id_C) loop
            Msg_Cont := Element (Id_C);
            Unchecked_Free (Msg_Cont);
            Next (Id_C);
         end loop;

         Self.Threaded := True;

         case Self.Sorted_By is
            when Sort_None => null;
            when Sort_Date =>
               Self.Sorted_By := Sort_None;
               Sort_By_Date (Self);
         end case;
      end if;
   end Thread_Messages;

   -----------------
   -- Is_Threaded --
   -----------------

   function Is_Threaded (Self : Stored_Mailbox) return Boolean is
   begin
      return Self.Threaded;
   end Is_Threaded;

begin
   --  Can't include two ASCII.LF, since officially mboxes should even use
   --  ASCII.CR & ASCII.LF, although most don't
   Compile (From_Pattern, ASCII.LF & "From ");
end GNATCOLL.Email.Mailboxes;
