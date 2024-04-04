with GNATCOLL.Directed_Graph; use GNATCOLL.Directed_Graph;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Assertions; use Ada.Assertions;
with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;
   use Node_Vectors;
   use Node_Sets;

   G : Directed_Graph;
   N1, N2, N3, N4, N5 : Node_Id;
   N : Node_Id;
   S : Node_Set;
begin
   --  Test Add_node method
   N1 := G.Add_Node;
   A.Assert (G.Contains (N1));

   --  Check that we cannot add a node with predecessor including itself
   --  This test makes assumption about how Node_Id are allocated
   begin
      N2 := G.Add_Node (Predecessors => (1 => 2));
      A.Assert (False, "Exception not raised");
   exception
      when DG_Error =>
         A.Assert (True);
      when others =>
         A.Assert (False, "Unknown exception");
   end;

   begin
      S.Clear;
      S.Include (2);
      N2 := G.Add_Node (Predecessors => S);
      A.Assert (False, "Exception not raised");
   exception
      when DG_Error =>
         A.Assert (True);
      when others =>
         A.Assert (False, "Unknown exception");
   end;

   --  Check that we cannot add non existing nodes as predecessors
   begin
      N2 := G.Add_Node (Predecessors => (5, 6));
      A.Assert (False, "Exception not raised");
   exception
      when DG_Error =>
         A.Assert (True);
      when others =>
         A.Assert (False, "Unknown exception");
   end;

   begin
      S.Clear;
      S.Include (5);
      S.Include (6);
      N2 := G.Add_Node (Predecessors => S);
      A.Assert (False, "Exception not raised");
   exception
      when DG_Error =>
         A.Assert (True);
      when others =>
         A.Assert (False, "Unknown exception");
   end;

   --  At this all node additions have failed except the first one. A failure
   --  on this test probably means that the pre-condition checks are done after
   --  allocating a new node.
   A.Assert (G.Length, 1);

   --  Add nodes with valid dependencies with both array and sets
   N2 := G.Add_Node (Predecessors => (1 => N1));
   N3 := G.Add_Node (Predecessors => (1 => N2));

   S.Clear;
   S.Include (N3);
   N4 := G.Add_Node (Predecessors => S);
   A.Assert (G.Contains (N4));

   --  Test Add_Predecessor(s) functions
   N5 := G.Add_Node;

   --  Cannot add a node in its own predecessors
   begin
      S.Clear;
      S.Include (N5);
      G.Add_Predecessors (N5, S);
      A.Assert (False, "Exception not raised");
   exception
      when DG_Error =>
         A.Assert (True);
      when others =>
         A.Assert (False, "Unknown exception");
   end;

   begin
      G.Add_Predecessor (N5, N5);
      A.Assert (False, "Exception not raised");
   exception
      when DG_Error =>
         A.Assert (True);
      when others =>
         A.Assert (False, "Unknown exception");
   end;

   --  Cannot add No_Node as predecessors
   begin
      S.Clear;
      S.Include (No_Node);
      S.Include (N4);
      G.Add_Predecessors (N5, S);
      A.Assert (False, "Exception not raised");
   exception
      when E : DG_Error =>
         A.Assert (True, Exception_Message (E));
      when others =>
         A.Assert (False, "Unknown exception");
   end;

   --  All nodes should exist
   begin
      G.Add_Predecessor (N5, 10);
      A.Assert (False, "Exception not raised");
   exception
      when DG_Error | Assertion_Error =>
         A.Assert (True);
      when E : others =>
         A.Assert (False, "Unknown exception: " &
                   Exception_Name (E) & " " & Exception_Message (E));
   end;

   begin
      G.Add_Predecessor (10, N5);
      A.Assert (False, "Exception not raised");
   exception
      when DG_Error | Assertion_Error =>
         A.Assert (True);
      when E : others =>
         A.Assert (False, "Unknown exception: " & Exception_Message (E));
   end;

   --  All nodes should exist
   declare
      Predecessors : Node_Set := Empty_Node_Set;
   begin
      Predecessors.Include (10);
      G.Add_Predecessors (N5, Predecessors);
      A.Assert (False, "Exception not raised");
   exception
      when DG_Error | Assertion_Error =>
         A.Assert (True);
      when E : others =>
         A.Assert (False, "Unknown exception: " &
                   Exception_Name (E) & " " & Exception_Message (E));
   end;

   declare
      Predecessors : Node_Set := Empty_Node_Set;
   begin
      Predecessors.Include (N5);
      G.Add_Predecessors (10, Predecessors);
      A.Assert (False, "Exception not raised");
   exception
      when DG_Error | Assertion_Error =>
         A.Assert (True);
      when E : others =>
         A.Assert (False, "Unknown exception: " & Exception_Message (E));
   end;

   declare
      Predecessors : Node_Set := Empty_Node_Set;
   begin
      Predecessors.Include (N4);
      G.Add_Predecessors (N5, Predecessors);
   end;

   --  N1 -> N2 -> N3 -> N4 -> N5
   A.Assert (not G.Has_Cycle);
   A.Assert (G.Shortest_Cycle = Empty_Node_Vector);

   declare
      Expected : constant Node_Vector := N1 & N2 & N3 & N4 & N5;
   begin
      --  Topological view

      A.Assert (Expected = G.Topological_Sort);
   end;

   G.Start_Iterator (False);
   A.Assert (G.Next (N) and then N = N1);
   A.Assert (G.Next (N) and then N = N2);
   A.Assert (G.Next (N) and then N = N3);
   A.Assert (G.Next (N) and then N = N4);
   A.Assert (G.Next (N) and then N = N5);
   A.Assert (not G.Next (N) and then N = No_Node);

   G.Start_Iterator (False);
   A.Assert (G.Next = N1);
   A.Assert (G.Next = N2);
   A.Assert (G.Next = N3);
   A.Assert (G.Next = N4);
   A.Assert (G.Next = N5);
   A.Assert (G.Next = No_Node);

   declare
      Iter : DAG_Iterator;
   begin
      Iter.Start_Iterator (G, False);
      G.Start_Iterator (False);
      A.Assert (Iter.Next (G) = N1);
      A.Assert (Iter.Next (G) = N2);
      A.Assert (Iter.Next (G) = N3);
      A.Assert (Iter.Next (G) = N4);
      A.Assert (Iter.Next (G) = N5);
      A.Assert (Iter.Next (G) = No_Node);
   end;

   --  Create a loop in the graph
   --  N1 -> N2 -> N3 -> N4 -> N1
   --                       -> N5

   declare
      Cycle : Node_Vector;
      S : Node_Set;
      Expected_Short_Cycle : Node_Set;
   begin
      Expected_Short_Cycle.Include (N1);
      Expected_Short_Cycle.Include (N2);
      Expected_Short_Cycle.Include (N3);
      Expected_Short_Cycle.Include (N4);

      G.Add_Predecessor (N1, N4);

      A.Assert (G.Has_Cycle);
      A.Assert (G.Topological_Sort = Node_Vectors.Empty_Vector);
      Cycle := G.Shortest_Cycle;
      for N of Cycle loop
         S.Include (N);
      end loop;
      A.Assert (S = Expected_Short_Cycle);
   end;

   --  When Enable_Visiting_State is not set, cycles are also detected during
   --  the graph iteration.

   begin
      declare
         Node : Node_Id;
         Status : Boolean;
      begin

         G.Start_Iterator (False);
         Status := G.Next (Node);

         --  Exception shall have been raised during the previous Next

         A.Assert (False, "Next returned " & Status'Img  & "Node=" & Node'Img);
      end;
   exception
      when others =>
         A.Assert (True);

   end;

   --  Reset the graph
   G.Clear;

   --    ->N2--
   --   /      \
   --  N1      N4
   --   \      /
   --    ->N3--

   N1 := G.Add_Node;
   N2 := G.Add_Node (Predecessors => (1 => N1));
   N3 := G.Add_Node (Predecessors => (1 => N1));
   N4 := G.Add_Node (Predecessors => (N3, N2));

   --  Visiting state disabled

   declare
      Node   : Node_Id;
      Status : Boolean;
   begin
      A.Assert (not G.Iterator_Started);
      G.Start_Iterator (False);
      A.Assert (G.Iterator_Started);

      A.Assert (G.Next (Node) and then Node = N1);
      A.Assert (G.Visiting_Nodes = Node_Sets.Empty_Set);

      for Unused in 1 .. 2 loop
         Status := G.Next (Node);
         A.Assert (Status and then (Node = N2 or else Node = N3));
      end loop;

      A.Assert (G.Next (Node) and then Node = N4);
      A.Assert (not G.Next (Node) and then Node = No_Node);
   end;

   --  Visiting state enabled

   declare
      Save_1, Save_2, Node : Node_Id;
      Status : Boolean;
   begin

      G.Start_Iterator (True);

      A.Assert (G.Next (Node) and then Node = N1);

      A.Assert (G.Next (Node) and Node = No_Node);
      G.Complete_Visit (N1);

      --  Check that we can not complete a non-visited node

      begin
         G.Complete_Visit (N4);
         A.Assert (False, "Exception not raised");
      exception
      when DG_Error | Assertion_Error =>
         A.Assert (True);
      when E : others =>
         A.Assert (False, "Unknown exception: " & Exception_Message (E));
      end;

      Status := G.Next (Save_1);
      A.Assert (Status and then (Save_1 = N2 or else Save_1 = N3));
      Status := G.Next (Save_2);
      A.Assert (Status and then (Save_2 = N2 or else Save_2 = N3));

      for Visiting_Node of G.Visiting_Nodes loop
         A.Assert (Visiting_Node = N2 or else Visiting_Node = N3);
      end loop;

      --  Visiting nodes need to be left

      A.Assert (G.Next (Node) and then Node = No_Node);
      G.Complete_Visit (Save_1);
      A.Assert (G.Next (Node) and then Node = No_Node);
      A.Assert (G.Visiting_Nodes.First_Element = Save_2);

      G.Complete_Visit (Save_2);
      A.Assert (G.Visiting_Nodes = Node_Sets.Empty_Set);

      --  Check that completing an already visited node has no effect

      G.Complete_Visit (Save_2);

      --  Last job is now available

      A.Assert (G.Next (Node) and then Node = N4);
      A.Assert (not G.Next (Node) and then Node = No_Node);
   end;

   --  Test node addition during an iteration

   declare
      Node   : Node_Id;
   begin
      G.Clear;

      --  N1 --> N2 --> N3
      --   \
      --    ---> N4

      N1 := G.Add_Node;
      N2 := G.Add_Node (Predecessors => (1 => N1));
      N3 := G.Add_Node (Predecessors => (1 => N2));
      N4 := G.Add_Node (Predecessors => (1 => N1));

      G.Start_Iterator (True);

      A.Assert (G.Next (Node) and then Node = N1);
      G.Complete_Visit (N1);
      A.Assert (G.Next (Node) and then Node = N2);
      G.Complete_Visit (N2);

      --  N1 --> N2 -------> N3
      --   |\--> N4 ------/
      --   |     \       /
      --    \-----> N5 -/

      N5 := G.Add_Node (Predecessors => (N1, N4));
      G.Add_Predecessor (N3, N5);

      A.Assert (G.Next (Node) and then Node = N4);
      A.Assert (G.Next (Node) and then Node = No_Node);
      G.Complete_Visit (N4);

      A.Assert (G.Next (Node) and then Node = N5);
      A.Assert (G.Next (Node) and then Node = No_Node);
      G.Complete_Visit (N5);

      A.Assert (G.Next (Node) and then Node = N3);
      G.Complete_Visit (N3);

      A.Assert (not G.Next (Node) and then Node = No_Node);
   end;

   --  Test shortest path

   G.Clear;

   --  N1 ----------> N2 --> N3
   --   \             /
   --    ---> N4 -> N5

   N1 := G.Add_Node;
   N4 := G.Add_Node (Predecessors => (1 => N1));
   N5 := G.Add_Node (Predecessors => (1 => N4));
   N2 := G.Add_Node (Predecessors => (N1, N5));
   N3 := G.Add_Node (Predecessors => (1 => N2));

   declare
      Expected : constant Node_Vector := N1 & N2 & N3;
   begin
      A.Assert (Expected = G.Shortest_Path (N1, N3));
   end;

   A.Assert (G.Shortest_Path (N3, N1) = Empty_Node_Vector);

   return A.Report;
end Test;
