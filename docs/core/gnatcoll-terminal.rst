*************************************
**Terminal**: controlling the console
*************************************

.. highlight:: ada

Applications generally provide user feedback either via full-fledge
graphical interfaces, or via a simpler, console-based output.

The basic support for console-based output is provided directly via
`Ada.Text_IO`. But more advanced features are highly system-dependent,
and somewhat tricky to develop.

The package `GNATCOLL.Terminal` provide cross-platform support for
manipulating colors in terminals, as well as a few basic cursor
manipulation subprograms.

Colors
======

Most modern terminals support color output, generally with a limit set of
colors. On Unix systems, these colors are set by using escape sequences in the
output; on Windows systems, these are manipulated by calling functions on a
file handle.

GNATCOLL will automatically try to guess whether its output is sent to a color
enabled terminal. In general, this will be true when outputing to standard
output or standard error, and false when outputing to files or to pipes.
You can override this default value to force either color support or
black-and-white support.

Here is an example::

   with Ada.Text_IO;        use Ada.Text_IO;
   with GNATCOLL.Terminal;  use GNATCOLL.Terminal;

   procedure Test_Colors is
      Info : Terminal_Info;
   begin
      Info.Init_For_Stdout (Auto);

      Info.Set_Color (Standard_Output, Blue, Yellow);
      Put_Line ("A blue on yellow line");

      Info.Set_Color (Standard_Output, Style => Reset_All);
      Put_Line ("Back to standard colors -- much better");
   end Test_Colors;

Cursors
=======

It is often useful for an application to display some progress indicator during
long operations. `GNATCOLL.Terminal` provides a limit set of subprograms to do
so, as in::

   with Ada.Text_IO;        use Ada.Text_IO;
   with GNATCOLL.Terminal;  use GNATCOLL.Terminal;

   procedure Test_Colors is
      Info : Terminal_Info;
   begin
      Info.Init_For_Stdout (Auto);
      for J in 1 .. 1_000 loop
         if J mod 10 = 0 then
            Put ("Processing file" & J'Img & " with long name");
         else
            Put ("Processing file" & J'Img);
         end if;
         delay 0.1;
         Info.Beginning_Of_Line;
         Info.Clear_To_End_Of_Line;
      end loop;
   end Test_Colors;         


