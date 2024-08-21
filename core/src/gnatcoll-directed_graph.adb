--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Ordered_Maps;

package body GNATCOLL.Directed_Graph is

   use type Ada.Containers.Count_Type;

   package Node_Int_Maps is new Ada.Containers.Ordered_Maps (Node_Id, Natural);
   subtype Node_Int_Map is Node_Int_Maps.Map;

   function Allocate_Node (Self : in out Directed_Graph) return Node_Id;
   --  Allocate a new node and return its ID

   function Min (List : Node_Sets.Set; Map : Node_Int_Map) return Node_Id;
   --  Return the key that corresponds to the minimum value in the map

   procedure Update_Cached_Data (Self : in out Directed_Graph);
   --  Compute the topological sort, store it in a cache and detect cycles.
   --  If cycles are detected, then the topological sort is aborted.

   procedure Internal_Add_Predecessor
      (Self : in out Directed_Graph; Node : Node_Id; Predecessor : Node_Id)
   with Inline => True;
   --  Add a predecessor to a given node. Note that this is the responsability
   --  of the caller to check node validity.

   function Contains (Self : Directed_Graph; Nodes : Node_Set)
      return Boolean
   with Inline => True;

   procedure Increment (N : in out Natural)
   with Inline_Always => True;

   --------------
   -- Add_Node --
   --------------

   function Add_Node
     (Self         : in out Directed_Graph;
      Predecessors : Node_Set := Empty_Node_Set)
     return Node_Id
   is
   begin
      --  Perform all checks before inclusion

      if not Self.Contains (Predecessors) then
         raise DG_Error with "Invalid predecessors";
      end if;

      --  Prerequisites are met, node can be created safely

      declare
         Node : constant Node_Id := Self.Allocate_Node;
      begin
         for Predecessor of Predecessors loop
            Self.Internal_Add_Predecessor (Node, Predecessor);
         end loop;

         return Node;
      end;
   end Add_Node;

   function Add_Node
     (Self         : in out Directed_Graph;
      Predecessors : Node_Array) return Node_Id
   is
   begin
      --  Perform all checks before inclusion

      for Pred of Predecessors loop
         if not Self.Contains (Pred) then
            raise DG_Error with "Invalid predecessors";
         end if;
      end loop;

      --  Prerequisites are met, node can be created safely

      declare
         Node : constant Node_Id := Self.Allocate_Node;
      begin
         --  Add them

         for Pred of Predecessors loop
            Self.Internal_Add_Predecessor (Node, Pred);
         end loop;

         return Node;
      end;
   end Add_Node;

   ---------------------
   -- Add_Predecessor --
   ---------------------

   procedure Add_Predecessor
     (Self        : in out Directed_Graph;
      Node        : Node_Id;
      Predecessor : Node_Id)
   is
      Prev_Length : Ada.Containers.Count_Type;
   begin
      if not Self.Contains (Node) then
         raise DG_Error with "Non existing node";
      end if;

      if not Self.Contains (Predecessor) then
         raise DG_Error with "Invalid predecessor";
      end if;

      if Predecessor = Node then
         raise DG_Error with "Predecessor cannot be the node itself";
      end if;

      Prev_Length := Self.Predecessors (Integer (Node)).Length;
      Self.Internal_Add_Predecessor (Node, Predecessor);

      if Self.Predecessors (Integer (Node)).Length > Prev_Length then
         Self.Is_Cache_Valid := False;
         Self.Update_List.Append (Node);
      end if;
   end Add_Predecessor;

   ----------------------
   -- Add_Predecessors --
   ----------------------

   procedure Add_Predecessors
     (Self         : in out Directed_Graph;
      Node         : Node_Id;
      Predecessors : Node_Set := Empty_Node_Set)
   is
      Prev_Length : Ada.Containers.Count_Type;
   begin
      if not Self.Contains (Node) then
         raise DG_Error with "Non existing node";
      end if;

      if not Self.Contains (Predecessors) then
         raise DG_Error with "Invalid predecessors";
      end if;

      if Predecessors.Contains (Node) then
         raise DG_Error with "Predecessor cannot be the node itself";
      end if;

      Prev_Length := Self.Predecessors (Integer (Node)).Length;

      for Pred of Predecessors loop
         Self.Internal_Add_Predecessor (Node, Pred);
      end loop;

      if Self.Predecessors (Integer (Node)).Length > Prev_Length then
         Self.Is_Cache_Valid := False;
         Self.Update_List.Append (Node);
      end if;

   end Add_Predecessors;

   --------------------
   --  Allocate_Node --
   --------------------

   function Allocate_Node
     (Self : in out Directed_Graph) return Node_Id
   is
      Node : Node_Id;
   begin
      if Self.Next_Free_Node = No_Node then
         raise DG_Error
            with "Graph cannot contain more than 2 ^ 32 - 1 element";
      end if;

      Node := Self.Next_Free_Node;

      if Self.Next_Free_Node < Node_Id'Last then
         Self.Next_Free_Node := Self.Next_Free_Node + 1;
      else
         --  The maximum number of nodes has been reached

         Self.Next_Free_Node := No_Node;
      end if;

      --  As no deletion is allowed doing an append to Predecessors and
      --  Successors works.

      Self.Predecessors.Append (Empty_Node_Set);
      Self.Successors.Append (Empty_Node_Set);

      Self.Is_Cache_Valid := False;

      return Node;
   end Allocate_Node;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Directed_Graph)  is
   begin
      Self.Next_Free_Node := First_Free_Node;
      Self.Predecessors.Clear;
      Self.Successors.Clear;
      Self.Sort_Cache.Clear;
      Self.Is_Cache_Valid := True;
      Self.Iterator.Started := False;
   end Clear;

   --------------------
   -- Complete_Visit --
   --------------------

   procedure Complete_Visit
      (Self  : in out DAG_Iterator'Class;
       Graph : Directed_Graph'Class;
       Node  : Node_Id)
   is
   begin

      --  Note that if the node is not in either Non_Visited or Visiting then
      --  it means the node has been visited.

      if Self.Non_Visited.Contains (Node) then
         raise DG_Error
            with "node" & Node'Img & "cannot be marked visited";
      elsif Self.Visiting.Contains (Node) then
         Self.Visiting.Delete (Node);

         for Successor of Graph.Successors (Positive (Node)) loop
            Increment (Self.Visited_Predecessors (Integer (Successor)));
         end loop;
      end if;
   end Complete_Visit;

   procedure Complete_Visit
     (Self : in out Directed_Graph'Class; Node : Node_Id)
   is
   begin
      Complete_Visit (Self.Iterator, Self, Node);
   end Complete_Visit;

   --------------
   -- Contains --
   --------------

   function Contains (Self : Directed_Graph; Nodes : Node_Set)
      return Boolean
   is
   begin

      --  As the graph structure does not support deletion of elements checking
      --  the first and last element of an ordered set is enough to check set
      --  validity.

      return Nodes.Length = 0 or else
         (Self.Contains (Nodes.Last_Element) and then
          Self.Contains (Nodes.First_Element));
   end Contains;

   function Contains (Self : Directed_Graph; Node : Node_Id) return Boolean
   is
   begin
      return Node > 0 and then Node < Self.Next_Free_Node;
   end Contains;

   ---------------
   -- Has_Cycle --
   ---------------

   function Has_Cycle (Self : in out Directed_Graph) return Boolean
   is
   begin
      if not Self.Is_Cache_Valid then
         Self.Update_Cached_Data;
      end if;

      return Self.Has_Cycle;
   end Has_Cycle;

   ---------------
   -- Increment --
   ---------------

   procedure Increment (N : in out Natural) is
   begin
      N := N + 1;
   end Increment;

   ------------------------------
   -- Internal_Add_Predecessor --
   ------------------------------

   procedure Internal_Add_Predecessor
      (Self : in out Directed_Graph; Node : Node_Id; Predecessor : Node_Id)
   is
   begin
      Self.Predecessors (Integer (Node)).Include (Predecessor);
      Self.Successors (Integer (Predecessor)).Include (Node);
   end Internal_Add_Predecessor;

   ----------------------
   -- Iterator_Started --
   ----------------------

   function Iterator_Started (Self : DAG_Iterator'Class) return Boolean
   is
   begin
      return Self.Started;
   end Iterator_Started;

   function Iterator_Started (Self : Directed_Graph) return Boolean
   is
   begin
      return Self.Iterator.Started;
   end Iterator_Started;

   ------------
   -- Length --
   ------------

   function Length (Self : Directed_Graph'Class) return Natural
   is
   begin
      return Natural (Self.Next_Free_Node - 1);
   end Length;

   ---------
   -- Min --
   ---------

   function Min (List : Node_Sets.Set; Map : Node_Int_Map) return Node_Id
   is
      Result : Node_Id;
      Value  : Integer := Integer'Last;
   begin
      for Key of List loop
         if Value > Map.Element (Key) then
            Value := Map.Element (Key);
            Result := Key;
         end if;
      end loop;

      return Result;
   end Min;

   ----------
   -- Next --
   ----------

   function Next
      (Self  : in out DAG_Iterator'Class;
       Graph : Directed_Graph'Class;
       Node  : out Node_Id)
      return Boolean
   is
   begin
      Node := No_Node;

      --  Check if new nodes were added

      if Self.Graph_Next_Free_Node < Graph.Next_Free_Node then

         for N in Self.Graph_Next_Free_Node .. Graph.Next_Free_Node - 1 loop

            --  Add the new node the list of non visited nodes
            Self.Non_Visited.Include (N);

            declare
               Visited_Pred : Integer := 0;
            begin
               --  Update the number of visited predecessors for the new nodes
               --  by checking whether any of the predecessors has been visited
               for Pred of Graph.Predecessors (Integer (N)) loop
                  if not Self.Non_Visited.Contains (Pred) and then
                     not Self.Visiting.Contains (Pred)
                  then
                     Visited_Pred := Visited_Pred + 1;
                  end if;
               end loop;

               --  Extend the visited predecessor array with the information of
               --  the new node
               Self.Visited_Predecessors.Append (Visited_Pred);
            end;
         end loop;

         --  Keep track that the new node have been taken into account.
         Self.Graph_Next_Free_Node := Graph.Next_Free_Node;
      end if;

      --  Check if new predecessors were added on existing nodes
      if Self.Graph_Update_List_Last < Graph.Update_List.Length then
         for Idx in Self.Graph_Update_List_Last + 1 .. Graph.Update_List.Length
         loop
            declare
               N : constant Node_Id := Graph.Update_List (Integer (Idx));
               Visited_Pred : Integer := 0;
            begin
               --  Update the number of visited predecessors for the updated
               --  nodes by checking whether any of the predecessors has been
               --  visited
               for Pred of Graph.Predecessors (Integer (N)) loop
                  if not Self.Non_Visited.Contains (Pred) and then
                     not Self.Visiting.Contains (Pred)
                  then
                     Visited_Pred := Visited_Pred + 1;
                  end if;
               end loop;

               Self.Visited_Predecessors (Integer (N)) := Visited_Pred;
            end;
         end loop;

         --  Update marker
         Self.Graph_Update_List_Last := Graph.Update_List.Length;
      end if;

      --  If all nodes have been visited it means that we have reached the
      --  end of the iteration.

      if Node_Sets.Length (Self.Non_Visited) = 0 then
         Node := No_Node;

         return False;
      end if;

      --  Otherwise try to find a node for which all predecessors have been
      --  visited.

      for N of Self.Non_Visited loop
         if Integer (Graph.Predecessors (Integer (N)).Length)
            - Self.Visited_Predecessors (Integer (N)) = 0
         then
            Node := N;
            exit;
         end if;
      end loop;

      if Node = No_Node then
         if not Self.Enable_Visiting_State then

            --  This means there is a cycle in the graph

            raise DG_Error with "cycle detected";
         else
            return True;
         end if;
      end if;

      Self.Non_Visited.Delete (Node);

      if Self.Enable_Visiting_State then
         Self.Visiting.Include (Node);
      else

         --  Found vertex is not blocking anymore their successors

         for Successor_Node of Graph.Successors (Integer (Node)) loop
            Increment (Self.Visited_Predecessors (Integer (Successor_Node)));
         end loop;
      end if;

      return True;
   end Next;

   function Next
      (Self  : in out DAG_Iterator'Class;
       Graph : Directed_Graph'Class)
      return Node_Id
   is
      Status : Boolean;
      pragma Unreferenced (Status);

      Result : Node_Id;
   begin
      Status := Next (Self, Graph => Graph, Node => Result);

      return Result;
   end Next;

   function Next (Self : in out Directed_Graph'Class) return Node_Id is
   begin
      return Next (Self.Iterator, Self);
   end Next;

   function Next
     (Self : in out Directed_Graph'Class; Node : out Node_Id) return Boolean
   is
   begin
      return Next (Self.Iterator, Self, Node);
   end Next;

   ---------------------
   -- Shortest_Cycle --
   ---------------------

   function Shortest_Cycle (Self : in out Directed_Graph) return Node_Vector
   is
      Result : Node_Vector;
   begin

      if not Self.Is_Cache_Valid then
         Self.Update_Cached_Data;
      end if;

      if not Self.Has_Cycle then
         return Empty_Node_Vector;
      end if;

      for Node in 1 .. Self.Next_Free_Node - 1 loop
         Result := Shortest_Path (Self, Node, Node);
         if not Result.Is_Empty then
            return Result;
         end if;
      end loop;

      raise DG_Error with "Has_Cycle set but no cycle found";
   end Shortest_Cycle;

   -------------------
   -- Shortest_Path --
   -------------------

   function Shortest_Path
     (Self           : Directed_Graph;
      Source, Target : Node_Id) return Node_Vectors.Vector
   is

      package Node_Node_Maps is new Ada.Containers.Ordered_Maps
        (Node_Id, Node_Id);

      Infinite    : constant Natural := Natural (Self.Next_Free_Node);
      --  Maximum distance between two vertices is the number of nodes in the
      --  graph - 1, unless Source and Target are equal in which case the
      --  maximum possible distance is the number of nodes. So infinity is
      --  Length (Nodes) + 1.

      Dist        : Node_Int_Map;
      --  This map will keep track of minimal distance between vertices and the
      --  source.

      Prev        : Node_Node_Maps.Map;
      --  This keeps track of the minimum distance

      Non_Visited : Node_Sets.Set;
      --  Non visited nodes

      T_Node        : Node_Id renames Target;
      S_Node        : Node_Id := Source;

      U, V        : Node_Id;
      Alt         : Natural;
      Result      : Node_Vectors.Vector;

   begin
      --  We use the Dijkstra algorithm to compute the shortest path.
      --  Note that this is a slight variation so that the algorithm
      --  can be used to compute shortest cycle on a given node.

      --  Initialize Dist:

      for Node in 1 .. Self.Next_Free_Node - 1 loop
         if Node = T_Node then
            --  Only known distance at startup
            Dist.Insert (Node, 0);
         else
            Dist.Insert (Node, Infinite);
         end if;
      end loop;

      --  Initialize Prev:

      for Node in 1 .. Self.Next_Free_Node - 1 loop
         Prev.Insert (Node, No_Node);
      end loop;

      for Node in 1 .. Self.Next_Free_Node  - 1 loop
         Non_Visited.Insert (Node);
      end loop;

      if S_Node = T_Node then

         --  If Source is equal to target, default Dijkstra algorithm does
         --  not work. Add a fake node and use it as target. When iterating
         --  on predecessors, replace all occurrences of sources to that node.
         --  If we find a path between that node and the source, it means we
         --  have our shortest cycle.

         Dist.Insert (No_Node, Infinite);
         Prev.Insert (No_Node, No_Node);
         Non_Visited.Insert (No_Node);
         S_Node := No_Node;
      end if;

      while not Non_Visited.Is_Empty loop
         U := Min (Non_Visited, Dist);
         Non_Visited.Delete (U);

         if U = S_Node then

            --  We found the shortest path

            exit;

         elsif U /= No_Node then
            for U_Pred of Self.Predecessors (Integer (U)) loop
               if S_Node = No_Node and then U_Pred = T_Node then
                  --  Handle cycle detection case

                  V := No_Node;
               else
                  V := U_Pred;
               end if;

               Alt := Dist.Element (U) + 1;

               if Alt < Dist.Element (V) then
                  Dist.Replace (V, Alt);
                  Prev.Replace (V, U);
               end if;
            end loop;
         end if;
      end loop;

      if Dist.Element (S_Node) = Infinite then

         --  No path between source and target

         return Node_Vectors.Empty_Vector;
      end if;

      U := S_Node;
      Result.Append (Source);

      while Prev.Element (U) /= No_Node loop
         U := Prev.Element (U);
         Result.Append (U);
      end loop;

      return Result;
   end Shortest_Path;

   --------------------
   -- Start_Iterator --
   --------------------

   procedure Start_Iterator
     (Self  : in out DAG_Iterator'Class;
      Graph : Directed_Graph'Class;
      Enable_Visiting_State : Boolean := False)
   is
      Non_Visited_Nodes : Node_Sets.Set := Node_Sets.Empty_Set;
      Visited_Predecessors : Pred_Num_Vector := Empty_Pred_Num_Vector;
      use Node_Sets;
   begin
      for Node_Id in 1 .. Graph.Next_Free_Node - 1 loop
         Non_Visited_Nodes.Include (Node_Id);
      end loop;

      --  ??? improve

      for Idx in 1 .. Graph.Next_Free_Node - 1 loop
         Visited_Predecessors.Append (0);
      end loop;

      Self.Non_Visited := Non_Visited_Nodes;
      Self.Visiting := Empty_Node_Set;
      Self.Visited_Predecessors := Visited_Predecessors;
      Self.Graph_Next_Free_Node := Graph.Next_Free_Node;
      Self.Enable_Visiting_State := Enable_Visiting_State;
      Self.Started := True;
   end Start_Iterator;

   procedure Start_Iterator
     (Self                  : in out Directed_Graph;
      Enable_Visiting_State : Boolean := False)
   is
   begin
      Start_Iterator (Self.Iterator, Self, Enable_Visiting_State);
   end Start_Iterator;

   ----------------------
   -- Topological_Sort --
   ----------------------

   function Topological_Sort (Self : in out Directed_Graph) return Node_Vector
   is
   begin
      if not Self.Is_Cache_Valid then
         Self.Update_Cached_Data;
      end if;

      return Self.Sort_Cache;
   end Topological_Sort;

   ------------------------
   -- Update_Cached_Data --
   ------------------------

   procedure Update_Cached_Data (Self : in out Directed_Graph)
   is
      Iterator : DAG_Iterator;
      Node     : Node_Id;
   begin
      Iterator.Start_Iterator
         (Graph                 => Self,
          Enable_Visiting_State => False);
      Self.Sort_Cache.Clear;

      begin
         while Next (Iterator, Self, Node) loop
            Self.Sort_Cache.Append (Node);
         end loop;

         Self.Has_Cycle := False;
      exception
         when DG_Error =>
            Self.Has_Cycle := True;
      end;

      Self.Is_Cache_Valid := True;
   end Update_Cached_Data;

   --------------------
   -- Visiting_Nodes --
   --------------------

   function Visiting_Nodes (Self : DAG_Iterator'Class) return Node_Sets.Set
   is
   begin
      return Self.Visiting.Copy;
   end Visiting_Nodes;

   function Visiting_Nodes (Self : Directed_Graph) return Node_Sets.Set
   is
   begin
      return Visiting_Nodes (Self.Iterator);
   end Visiting_Nodes;

end GNATCOLL.Directed_Graph;
