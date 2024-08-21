--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

package GNATCOLL.Directed_Graph is

   DG_Error : exception;

   type Directed_Graph is tagged limited private;
   --  An object representing a Direct Graph

   type DAG_Iterator is tagged limited private;
   --  An iterator on a directed acyclic graph. Note that each graph contains
   --  also its own iterator for convenience.

   type Node_Id is mod 2 ** 32 with Default_Value => 0;
   --  A node identifier in a graph structure. Limiting to 2^32 - 1 nodes is
   --  enough for all realistic case.

   No_Node : constant Node_Id := 0;
   --  Special node id value used to represents the absence of node id

   package Node_Vectors is new Ada.Containers.Vectors (Positive, Node_Id);
   subtype Node_Vector is Node_Vectors.Vector;
   Empty_Node_Vector : constant Node_Vector := Node_Vectors.Empty_Vector;

   package Node_Sets is new Ada.Containers.Ordered_Sets (Node_Id);
   subtype Node_Set is Node_Sets.Set;
   Empty_Node_Set : constant Node_Set := Node_Sets.Empty_Set;

   type Node_Array is array (Natural range <>) of Node_Id;

   function Length (Self : Directed_Graph'Class) return Natural
   with Inline => True;
   --  Return the size of the graph (i.e the number of nodes)

   function Add_Node
     (Self         : in out Directed_Graph;
      Predecessors : Node_Set := Empty_Node_Set)
     return Node_Id;
   --  Add a node to the graph and set its precedecessors. If the graph already
   --  contains 2 ^ 32 - 1 elements raise DG_Error. Otherwise return the
   --  Node_Id of the new node.
   --  Predecessors should contain only node ids of existing nodes.

   function Add_Node
     (Self         : in out Directed_Graph;
      Predecessors : Node_Array)
     return Node_Id;
   --  Same as previous function except that Predecessors are passed in an
   --  array. Note that duplicate Node_Ids in Predecessors are ignored.

   procedure Add_Predecessors
     (Self         : in out Directed_Graph;
      Node         : Node_Id;
      Predecessors : Node_Set := Empty_Node_Set);
   --  Add predecessors to an existing node. All node ids should refer to
   --  existing nodes.

   procedure Add_Predecessor
     (Self        : in out Directed_Graph;
      Node        : Node_Id;
      Predecessor : Node_Id);
   --  Add a predecessor to a node. Both nodes must belong to the graph

   function Contains (Self : Directed_Graph; Node : Node_Id) return Boolean
   with Inline => True;
   --  Return True if Self contains a node whose id is Node, False otherwise.

   function Has_Cycle (Self : in out Directed_Graph) return Boolean;
   --  Return True if the graph contains at least one cycle.

   function Shortest_Cycle (Self : in out Directed_Graph) return Node_Vector;
   --  Return the smallest circle, if any, or the empty vector

   function Topological_Sort (Self : in out Directed_Graph) return Node_Vector;
   --  Return the list of nodes in a topological order

   function Shortest_Path
     (Self           : Directed_Graph;
      Source, Target : Node_Id)
     return Node_Vector;
   --  Compute the shortest path between two vertices of the graph.
   --  If target equals to Source then the algorithm tries to find the
   --  shortest cycle including Source.
   --  If there is no path between the two vertices, then the return value is
   --  an empty vector.

   procedure Clear (Self : in out Directed_Graph);
   --  Reset the graph by removing all the nodes and stop its iterator if
   --  needed.

   --------------
   -- Iterator --
   --------------

   procedure Start_Iterator
     (Self  : in out DAG_Iterator'Class;
      Graph : Directed_Graph'Class;
      Enable_Visiting_State : Boolean := False);
   --  Initialize an iterator for the specified graph. If
   --  Enable_Visiting_State is set, then nodes states go through an
   --  intermediate state "Visiting" after the "Non_Visited" one. Nodes with
   --  the "Visiting" state are still blocking their predecessors but are not
   --  returned anymore by the Next function.
   --  The function "Complete" must be called to go from "Visiting" to
   --  "Visited" state. This mechanism can be useful for parallel tasking, as
   --  we do not want one task to keep going forward before one of its
   --  predecessor has not completed.
   --  If the iterator is already started, then it is restarted from scratch.

   function Next
      (Self  : in out DAG_Iterator'Class;
       Graph : Directed_Graph'Class;
       Node  : out Node_Id)
      return Boolean;
   --  Update the iterator to the next state.
   --
   --  The output is one of the following:
   --
   --  1- The function returns False. This means that the end of the iteration
   --     has been reached. In that case Node is always set to No_Node.
   --  2- The function returns True and Node is different from No_Node. In that
   --     case the node state is set to "visiting" or "visited" depending on
   --     how the iterator was initialized.
   --  3- The function returns True and Node is set to No_Node. This case only
   --     occurs when Enable_Visiting_State is set to True. This means that
   --     some nodes are in the visiting state and prevent the visiting of new
   --     nodes. The function will return No_Node until the some of the
   --     "visiting" nodes are marked as visited using Compilte_Visit method

   function Next
      (Self  : in out DAG_Iterator'Class;
       Graph : Directed_Graph'Class)
      return Node_Id;
   --  Update the iterator to the next state.
   --
   --  Return the next available node and set its state as visited or visiting
   --  (see Enable_Visiting_State parameter of Start_Iterator).
   --
   --  If Enable_Visiting_State was set to False the function will return
   --  No_Node on end of iteration
   --  If Enable_Visiting_State was set to True, then the function return
   --  No_Node on end of iteration or when some nodes in visiting state are
   --  blocking the visit of newer node. To do the distinction between the two
   --  state, calling Visiting_Nodes should be done. If the return set of
   --  visiting nodes is empty then it means that end of iteration was reached.

   procedure Complete_Visit
      (Self  : in out DAG_Iterator'Class;
       Graph : Directed_Graph'Class;
       Node  : Node_Id);
   --  Mark the current vertex as "Visited". Has no effect if the vertex
   --  state was not "Visiting".

   function Visiting_Nodes (Self : DAG_Iterator'Class) return Node_Set;
   --  Return the list of the Visiting nodes

   function Iterator_Started (Self : DAG_Iterator'Class) return Boolean
   with Inline => True;
   --  Return True if the iterator has been started

   -----------------------
   -- Internal Iterator --
   -----------------------

   --  Same iterator interface except that the graph's internal iterator
   --  is used.

   procedure Start_Iterator
     (Self                  : in out Directed_Graph;
      Enable_Visiting_State : Boolean := False);
   --  Initialize the iterator embedded with the graph. If
   --  Enable_Visiting_State is set, then nodes states go through an
   --  intermediate state "Visiting" after the "Non_Visited" one. Nodes with
   --  the "Visiting" state are still blocking their predecessors but are not
   --  returned anymore by the Next function.
   --  The function "Complete" must be called to go from "Visiting" to
   --  "Visited" state. This mechanism can be useful for parallel tasking, as
   --  we do not want one task to keep going forward before one of its
   --  predecessor has not completed.
   --  If the iterator is already started, then it is restarted from scratch.

   function Next
     (Self : in out Directed_Graph'Class; Node : out Node_Id) return Boolean;
   --  Update the internal iterator to the next state.
   --
   --  The output is one of the following:
   --
   --  1- The function returns False. This means that the end of the iteration
   --     has been reached. In that case Node is always set to No_Node.
   --  2- The function returns True and Node is different from No_Node. In that
   --     case the node state is set to "visiting" or "visited" depending on
   --     how the iterator was initialized.
   --  3- The function returns True and Node is set to No_Node. This case only
   --     occurs when Enable_Visiting_State is set to True. This means that
   --     some nodes are in the visiting state and prevent the visiting of new
   --     nodes. The function will return No_Node until the some of the
   --     "visiting" nodes are marked as visited using Compilte_Visit method

   function Next (Self : in out Directed_Graph'Class) return Node_Id;
   --  Update the internal iterator to the next state.
   --
   --  Return the next available node and set its state as visited or visiting
   --  (see Enable_Visiting_State parameter of Start_Iterator).
   --
   --  If Enable_Visiting_State was set to False the function will return
   --  No_Node on end of iteration
   --  If Enable_Visiting_State was set to True, then the function return
   --  No_Node on end of iteration or when some nodes in visiting state are
   --  blocking the visit of newer node. To do the distinction between the two
   --  state, calling Visiting_Nodes should be done. If the return set of
   --  visiting nodes is empty then it means that end of iteration was reached.

   procedure Complete_Visit
     (Self : in out Directed_Graph'Class; Node : Node_Id);
   --  Mark the current vertex as "Visited" within the internal iterator.
   --  Has no effect if the vertex state was not "Visiting".

   function Visiting_Nodes (Self : Directed_Graph) return Node_Set;
   --  Return the list of the Visiting nodes for the internal iterator

   function Iterator_Started (Self : Directed_Graph) return Boolean;
   --  Return True if the internal iterator has been started

private

   package Node_Set_Vectors is new Ada.Containers.Vectors
      (Positive, Node_Set, "=" => Node_Sets."=");
   subtype Node_Set_Vector is Node_Set_Vectors.Vector;
   Empty_Node_Set_Vector : constant Node_Set_Vector :=
      Node_Set_Vectors.Empty_Vector;

   package Pred_Num_Vectors is new Ada.Containers.Vectors (Positive, Natural);
   subtype Pred_Num_Vector is Pred_Num_Vectors.Vector;
   Empty_Pred_Num_Vector : constant Pred_Num_Vector :=
      Pred_Num_Vectors.Empty_Vector;

   First_Free_Node : constant := 1;

   type State is (Non_Visited, Visiting, Visited);
   --  Non_Visited : The vertex has not been iterated.
   --  Visiting    : Only used when Enable_Visiting_State is set. The vertex
   --                has been iterated but it has not been processed by the
   --                graph iterator caller. Use the "Complete" procedure to go
   --                from the "Visiting" to the "Visited" state.
   --  Visited     : The vertex has been iterated and processed.

   type DAG_Iterator is tagged limited record

      Non_Visited           : Node_Set := Empty_Node_Set;
      --  Nodes that have not been visited yet

      Visiting              : Node_Set := Empty_Node_Set;
      --  Nodes that are currently in visiting state

      Visited_Predecessors  : Pred_Num_Vector := Empty_Pred_Num_Vector;
      --  For each node of the graph, the number of predecessors that have been
      --  visited.

      Graph_Next_Free_Node  : Node_Id := No_Node;
      --  Used to detect addition of new nodes in the Graph linked to the
      --  iterator.

      Graph_Update_List_Last : Ada.Containers.Count_Type := 0;
      --  Used to detect addition of predecessors in existing nodes

      Enable_Visiting_State : Boolean := False;
      --  Indicate if there is an intermediate state "Visiting" between the
      --  "Non_Visited" and "Visited" ones.

      Started               : Boolean := False;

   end record;

   type Directed_Graph is tagged limited record
      Predecessors   : Node_Set_Vector;
      --  Set of predecessors for each node

      Successors     : Node_Set_Vector;
      --  Set of successors for each node

      Next_Free_Node : Node_Id := First_Free_Node;
      --  Next available Node_Id. As no deletion is permitted
      --  Next_Free_Node - 1 represents also the number of nodes in the graph.

      --  The following two attributes are cached on first computation
      Sort_Cache     : Node_Vectors.Vector;
      --  One topological sort of the graph's nodes

      Has_Cycle      : Boolean := False;
      --  Whether the graph contains a cycle

      Is_Cache_Valid : Boolean := True;
      --  Whether the Sort_Cache is valid. True by default with everything
      --  empty.

      Iterator       : DAG_Iterator;
      --  Internal iterator

      Update_List : Node_Vectors.Vector;
      --  Keep track of add_predecessor operations. Needed in order to track
      --  changes during iterations.
   end record;

end GNATCOLL.Directed_Graph;
