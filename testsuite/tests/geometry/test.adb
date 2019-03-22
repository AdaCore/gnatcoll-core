------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Ada.Text_IO;
with GNATCOLL.Geometry;
with Test_Assert;

function Test return Integer is

   package IO renames Ada.Text_IO;
   package A renames Test_Assert;

   generic
      type Coordinate is digits <>;
      with package G is new GNATCOLL.Geometry (Coordinate);
   package Asserts is
      use type G.Point;

      Tolerance : constant G.Distance_Type := 1.0E-5;
      --  Tolerance when comparing distances

      procedure Put (P : G.Point);
      --  Put image of a point on stdout

      procedure Put (C : G.Circle);
      --  Put image of a circle on stdout

      procedure Assert (P1, P2 : G.Point; Error : String);
      --  Check if two points are equals

      procedure Assert_Inside
        (P          : G.Point;
         S          : G.Segment;
         On_Line    : Boolean := True;
         On_Segment : Boolean := True;
         Error      : String  := "");

      procedure Assert_Inside_Triangle
        (P      : G.Point;
         T      : G.Triangle;
         Result : Boolean := True;
         Error  : String  := "");

      procedure Assert_Intersection
        (S1, S2              : G.Segment;
         On_Line, On_Segment : G.Point;
         Error               : String);

      procedure Assert_Intersect_Triangle
        (T1, T2 : G.Triangle; Result : Boolean; Error : String);
      procedure Assert_Intersect_Rectangle
        (R1, R2 : G.Rectangle; Result : Boolean; Error : String);
      procedure Assert_Same_Side
        (P1, P2 : G.Point; S : G.Segment; Result : Boolean; Error : String);
      procedure Assert (D1, D2 : Coordinate'Base; Error : String);
      procedure Assert_Distance
         (P : G.Point; S : G.Segment; On_Line, On_Segment : Coordinate;
          Error : String);
      procedure Assert_Area
         (P : G.Polygon; D : Coordinate; Error : String);
      procedure Assert
         (C1, C2 : G.Circle; Error : String);
   end Asserts;

   package body Asserts is

      ---------
      -- Put --
      ---------

      procedure Put (P : G.Point) is
      begin
         if P = G.No_Point then
            IO.Put ("(No point)");
         elsif P = G.Infinity_Points then
            IO.Put ("(Infinity of points)");
         else
            IO.Put ("(" & P.X'Img & "," & P.Y'Img & ")");
         end if;
      end Put;

      ---------
      -- Put --
      ---------

      procedure Put (C : G.Circle) is
      begin
         IO.Put ("(c="); Put (C.Center);
         IO.Put (" r=" & C.Radius'Img);
      end Put;

      ------------
      -- Assert --
      ------------

      procedure Assert (P1, P2 : G.Point; Error : String) is
         Success : constant Boolean := P1 = P2;
      begin
         A.Assert (Success, Error);
         if not Success then
            IO.Put ("expected: "); Put (P1); IO.New_Line;
            IO.Put ("got:      "); Put (P2); IO.New_Line;
         end if;
      end Assert;

      procedure Assert_Inside
        (P          : G.Point;
         S          : G.Segment;
         On_Line    : Boolean := True;
         On_Segment : Boolean := True;
         Error      : String  := "")
      is
         SR : constant G.Segment := (S (2), S (1));
      begin
         A.Assert (G.Inside (P, G.To_Line (S)) = On_Line,
                   Error & " (line test)");
         A.Assert (G.Inside (P, G.To_Line (SR)) = On_Line,
                   Error & " (reverse line test)");
         A.Assert (G.Inside (P, S) = On_Segment,
                 Error & " (segment test)");
         A.Assert (G.Inside (P, SR) = On_Segment,
                 Error & " (reverse segment test)");
      end Assert_Inside;

      procedure Assert_Inside_Triangle
        (P : G.Point; T : G.Triangle;
         Result : Boolean := True;
         Error  : String  := "")
      is
         T2 : constant G.Triangle := (T (3), T (2), T (1));
      begin
         A.Assert (G.Inside (P, T) = Result, Error & " (triangle test)");
         A.Assert (G.Inside (P, T2) = Result,
                   Error & " (reverse triangle test)");
         A.Assert (G.Inside (P, G.Polygon (T)) = Result,
                   Error & " (polygon test)");
         A.Assert (G.Inside (P, G.Polygon (T2)) = Result,
                   Error & " (reverse polygon test)");
      end Assert_Inside_Triangle;

      procedure Assert_Intersection
        (S1, S2     : G.Segment;
         On_Line    : G.Point;
         On_Segment : G.Point;
         Error      : String)
      is
      begin
         A.Assert (G.Intersection (G.To_Line (S1),
                   G.To_Line (S2)) = On_Line,
                   Error & " (line test)");
         A.Assert (G.Intersection (G.To_Line (S2),
                   G.To_Line (S1)) = On_Line,
                   Error & " (reverse line test)");
         A.Assert (G.Intersection (S1, S2) = On_Segment,
                   Error & " (segment test)");
         A.Assert (G.Intersection (S2, S1) = On_Segment,
                   Error & " (reverse segment test)");
      end Assert_Intersection;

      procedure Assert_Intersect_Triangle
        (T1, T2 : G.Triangle; Result : Boolean; Error : String)
      is
         T3 : constant G.Triangle := (T1 (2), T1 (3), T1 (1));
      begin
         A.Assert (G.Intersect (T1, T2) = Result, Error & " (test1)");
         A.Assert (G.Intersect (T2, T1) = Result, Error & " (test2)");
         A.Assert (G.Intersect (T3, T2) = Result, Error & " (test3)");
         A.Assert (G.Intersect (T2, T3) = Result, Error & " (test4)");
      end Assert_Intersect_Triangle;

      procedure Assert_Intersect_Rectangle
        (R1, R2 : G.Rectangle; Result : Boolean; Error : String)
      is
      begin
         A.Assert (G.Intersect (R1, R2) = Result, Error);
      end Assert_Intersect_Rectangle;

      procedure Assert_Same_Side
        (P1, P2 : G.Point; S : G.Segment; Result : Boolean; Error : String) is
      begin
         A.Assert (G.Same_Side (P1, P2, S) = Result,
                   Error & " (segment test)");
         A.Assert (G.Same_Side (P1, P2, G.To_Line (S)) = Result,
                   Error & " (line test)");
      exception
         when others =>
            A.Assert (False, Error & " (exception raised)");
      end Assert_Same_Side;

      procedure Assert (D1, D2 : Coordinate'Base; Error : String) is
         Success : constant Boolean := abs (D1 - D2) <= Tolerance;
      begin
         A.Assert (Success, Error);
         if not Success then
            IO.Put_Line ("expected: " & D1'Img);
            IO.Put_Line ("got:      " & D2'Img);
         end if;
      end Assert;

      procedure Assert_Distance
         (P : G.Point;
          S : G.Segment;
          On_Line, On_Segment : Coordinate;
          Error : String)
      is
         SR : constant G.Segment := (S (2), S (1));
      begin
         A.Assert (G.Distance (P, G.To_Line (S)) = On_Line,
                   Error & " (line test)");
         A.Assert (G.Distance (P, G.To_Line (SR)) = On_Line,
                 Error & " (reverse line test)");
         A.Assert (G.Distance (P, S) = On_Segment,
                 Error & " (segment test)");
         A.Assert (G.Distance (P, SR) = On_Segment,
                 Error & " (reverse segment test)");
      end Assert_Distance;

      procedure Assert_Area
         (P : G.Polygon; D : Coordinate; Error : String) is
      begin
         Assert (G.Area (P), abs (D), Error & " (poly test)");
         if P'Length = 3 then
            Assert (G.Area (G.Triangle (P)), D,
                    Error & " (triangle test)");
         end if;
      end Assert_Area;

      procedure Assert (C1, C2 : G.Circle; Error : String) is
         use type G.Circle;
         Success : constant Boolean := C1 = C2;
      begin
         A.Assert (Success, Error);
         if not Success then
            IO.Put ("expected: "); Put (C1); IO.New_Line;
            IO.Put ("got:      "); Put (C2); IO.New_Line;
         end if;
      end Assert;
   end Asserts;

   generic
      type Coordinate is digits <>;
   procedure Tests;

   procedure Tests is
      package G is new GNATCOLL.Geometry (Coordinate);
      package Assertions is new Asserts (Coordinate, G);
      use Assertions, G, G.Coordinate_Elementary_Functions;
   begin
      Assert_Inside ((2.0, 0.0), ((0.0, 0.0), (10.0, 0.0)), True, True,
                     "Point on vertical line");
      Assert_Inside ((-1.0, 0.0), ((0.0, 0.0), (10.0, 0.0)), True, False,
                     "Point on vertical line, but not on segment");
      Assert_Inside ((0.0, 0.0), ((0.0, 0.0), (10.0, 0.0)), True, True,
                     "Point on vertical line at one end");
      Assert_Inside ((0.0, 2.0), ((0.0, 0.0), (0.0, 10.0)), True, True,
                     "Point on horizontal line");
      Assert_Inside ((0.0, -1.0), ((0.0, 0.0), (0.0, 10.0)), True, False,
                     "Point on horizontal line, but not on segment");
      Assert_Inside ((0.0, 0.0), ((0.0, 0.0), (0.0, 10.0)), True, True,
                     "Point on horizontal line at one end");
      Assert_Inside ((0.0, 0.0), ((-2.0, 2.0), (2.0, 2.0)), False, False,
                     "Not on line nor segment");
      Assert_Intersection
         (((0.0, 0.0), (10.0, 0.0)), ((0.0, 0.0), (10.0, 0.0)),
          Infinity_Points, Infinity_Points,
          "Intersection of same horizontal line is infinite");
      Assert_Intersection
         (((0.0, 0.0), (10.0, 0.0)), ((0.0, 0.0), (5.0, 0.0)),
          Infinity_Points, Infinity_Points,
          "Intersection of overlapping horizontal line is infinite");
      Assert_Intersection
         (((0.0, 0.0), (0.0, 10.0)), ((0.0, 0.0), (0.0, 10.0)),
          Infinity_Points, Infinity_Points,
          "Intersection of same vertical line is infinite");
      Assert_Intersection
         (((0.0, 0.0), (0.0, 10.0)), ((0.0, 0.0), (0.0, 5.0)),
           Infinity_Points, Infinity_Points,
          "Intersection of overlapping vertical vectors is infinite");
      Assert_Intersection
         (((0.0, 0.0), (10.0, 0.0)), ((-2.0, 2.0), (2.0, 2.0)),
           No_Point, No_Point,
          "No intersection of parallel vectors");
      Assert_Intersection
         (((0.0, 0.0), (10.0, 0.0)), ((11.0, 0.0), (20.0, 0.0)),
           Infinity_Points, No_Point,
           "Intersection of aligned vectors");
      Assert_Intersection
         (((0.0, 0.0), (0.0, 10.0)), ((-2.0, 2.0), (2.0, 2.0)),
           Point'(0.0, 2.0), Point'(0.0, 2.0),
           "Simple segment intersection");
      Assert_Intersection
         (((0.0, 0.0), (0.0, 10.0)), ((-2.0, 0.0), (2.0, 0.0)),
           Point'(0.0, 0.0), Point'(0.0, 0.0),
           "Intersection at one end of the vectors");
      Assert_Intersection
         (((0.0, 0.0), (10.0, 0.0)), ((-2.0, 2.0), (2.0, 2.0)),
           No_Point, No_Point,
           "Parallel lines have no intersection");
      Assert_Distance
         ((0.0, 0.0), ((-2.0, 2.0), (2.0, 2.0)), 2.0, 2.0,
           "Simple distance to line");
      Assert_Distance
         ((0.0, 2.0), ((-2.0, 2.0), (2.0, 2.0)), 0.0, 0.0,
           "Distance when point on line");
      Assert_Distance
         ((-3.0, 2.0), ((-2.0, 2.0), (2.0, 2.0)), 0.0, 1.0,
           "Distance when point on line, not on segment");
      Assert_Distance
         ((-3.0, 0.0), ((-2.0, 2.0), (2.0, 2.0)), 2.0, Sqrt (5.0),
           "Distance when orthogonal projection not on segment");
      Assert_Area
         (((0.0, 0.0), (4.0, 0.0), (0.0, 3.0)), 6.0,
           "Area of a counter-clockwise triangle");
      Assert_Area
         (((0.0, 0.0), (0.0, 3.0), (4.0, 0.0)), 6.0,
           "Area of clockwise triangle");
      Assert_Area
         (((0.0, 0.0), (0.0, 3.0), (2.0, 3.0), (2.0, 4.0),
           (3.0, 4.0), (3.0, 0.0)), 10.0,
           "Area for two squares (non-convex)");
      Assert
         (To_Circle ((0.0, 0.0), (1.0, 0.0), (0.0, 1.0)),
          ((0.5, 0.5), Sqrt (0.5)),
          "Circle from point");
      Assert_Inside_Triangle
         ((0.0, 0.0), ((0.0, 0.0), (0.0, 1.0), (1.0, 0.0)),
          True, "Point on boundary of triangle");
      Assert_Inside_Triangle
         ((0.1, 0.1), ((0.0, 0.0), (0.0, 1.0), (1.0, 0.0)),
          True, "Point inside triangle");
      Assert_Inside_Triangle
         ((0.5, 1.0), ((0.0, 0.0), (0.0, 1.0), (1.0, 0.0)),
          False, "Point outside triangle");
      A.Assert
         (G.Inside ((2.5, 3.5),
                    Poly => ((0.0, 0.0), (0.0, 3.0), (2.0, 3.0), (2.0, 4.0),
                   (3.0, 4.0), (3.0, 0.0))),
          "Point inside non-convex polygon");
      A.Assert
         (G.Inside ((2.0, 2.5),
                  Poly => ((0.0, 0.0), (0.0, 3.0), (2.0, 3.0), (2.0, 4.0),
                   (3.0, 4.0), (3.0, 0.0))),
          "Point inside non-convex polygon, vertically below vertex");
      Assert_Same_Side
         ((0.0, 0.0), (4.0, 0.0), ((-2.0, 2.0), (2.0, 2.0)), True,
          "Same side of horizontal line");
      Assert_Same_Side
         ((0.0, 0.0), (0.0, 4.0), ((-2.0, 2.0), (-2.0, -2.0)), True,
          "Same side of vertical line");
      Assert_Same_Side
         ((0.0, 0.0), (4.0, 0.0), ((2.0, 2.0), (2.0, -2.0)), False,
          "Not same side of vertical line");
      Assert
         (Centroid (((0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0))),
          (0.5, 0.5),
          "Centroid of rectangle");
      Assert
         (Centroid (((0.0, 0.0), (1.0, 0.0), (0.0, 1.0))),
          (1.0 / 3.0, 1.0 / 3.0),
          "Centroid of triangle");
      Assert_Intersect_Triangle
         (((0.0, 0.0), (1.0, 0.0), (0.0, 1.0)),
          ((0.5, 0.0), (1.5, 0.0), (0.5, 1.0)),
          True, "Intersection of two triangles");
      Assert_Intersect_Triangle
         (((0.0, 0.0), (1.0, 0.0), (0.0, 1.0)),
          ((1.0, 0.0), (2.0, 0.0), (1.0, 1.0)),
          True, "Intersection with one common vertex");
      Assert_Intersect_Triangle
         (((0.0, 0.0), (1.0, 0.0), (0.0, 1.0)),
          ((1.1, 0.0), (2.1, 0.0), (1.1, 1.0)),
          False, "No Intersection");
      Assert_Intersect_Rectangle
         (((0.0, 0.0), (1.0, 1.0)),
          ((0.5, 0.0), (1.5, 2.0)),
          True, "Intersection of rectangles");
      Assert_Intersect_Rectangle
         (((0.0, 0.0), (1.0, 1.0)),
          ((0.5, 1.0), (1.5, 2.0)),
          True, "Intersection of rectangles with common edge");
      Assert_Intersect_Rectangle
         (((0.0, 0.0), (1.0, 1.0)),
          ((0.5, 1.1), (1.5, 2.0)),
          False, "No intersection of rectangles");

      --  From the documentation of Boost geometry
      --  http://geometrylibrary.geodan.nl/

      declare
         P1 : constant Point := (1.0, 1.0);
         P2 : constant Point := (2.0, 3.0);
         P3 : constant Point := (3.7, 2.0);
         P  : constant Polygon :=
            ((2.0, 1.3), (4.1, 3.0), (5.3, 2.6), (2.9, 0.7));
      begin
         Assert (2.23607, Distance (P1, P2), "Distance P1-P2");
         Assert (3.015,   Area (P), "Area of polygon");
         A.Assert (Inside (P3, P), "Point inside polygon");
         Assert (1.04403, Distance (P1, P), "Distance point-polygon");
      end;
   end Tests;

begin
   IO.Put_Line ("==== Coordinates = Float ====");
   declare
      subtype Coordinate is Float;
      procedure Float_Tests is new Tests (Coordinate);
   begin
      Float_Tests;
   end;

   --  Check we can instantiate with more restricted types

   IO.Put_Line ("==== Coordinates = -500.0 .. 500.0 ====");
   declare
      type Coordinate is digits 1 range -500.0 .. 500.0;
      procedure Float_Tests is new Tests (Coordinate);
   begin
      Float_Tests;
   end;

   IO.Put_Line ("==== Coordinates = -500.0 .. 200.0 ====");
   declare
      type Coordinate is digits 1 range -500.0 .. -200.0;
      package Geom is new GNATCOLL.Geometry (Coordinate);
      package Assertions is new Asserts (Coordinate, Geom);
      use Geom, Assertions;
      P1 : constant Point := (-401.0, -401.0);
      P2 : constant Point := (-402.0, -403.0);
   begin
      Assert (2.23607, Distance (P1, P2), "Distance P1-P2");
   end;

   return A.Report;
end Test;
