********************************************
**Geometry**: primitive geometric operations
********************************************

.. highlight:: ada

GNATColl provides the package `GNATCOLL.Geometry`. This
package includes a number of primitive operations on geometric figures
like points, segments, lines, circles, rectangles and polygons.
In particular, you can compute their intersections, the distances,...

This package is generic, so that you can specify the type of coordinates
you wish to handle::

   declare
      package Float_Geometry is new GNATCOLL.Geometry (Float);
      use Float_Geometry;

      P1 : constant Point := (1.0, 1.0);
      P2 : constant Point := (2.0, 3.0);
   begin
      Put_Line ("Distance P1-P2 is" & Distance (P1, P2)'Img);
      --  Will print 2.23607
   end;
  

Or some operations involving a polygon::

     declare
        P3 : constant Point := (3.7, 2.0);
        P  : constant Polygon :=
           ((2.0, 1.3), (4.1, 3.0), (5.3, 2.6), (2.9, 0.7), (2.0, 1.3));
     begin
        Put_Line ("Area of polygon:" & Area (P));   --   3.015
        Put_Line ("P3 inside polygon ? " & Inside (P3, P)'Img);  --  True
     end;
