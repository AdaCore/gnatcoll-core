***************************************
**Readline**: interactive command line
***************************************

.. highlight:: ada

`gnatcoll` provides an interface to the `readline` library.

When this library is available on the system, it provides various features
to enhance command line support in tools.

In particular, it provides various keybindings to make editing more
comfortable than Ada.Text_IO.Get_Line. For instance, it is possible to use
backspace to edit what you have just typed. It is also possible to move forward
or backward by word, go to the start or end of line,...

`readline` also provides support for completion: by using the :kbd:`tab` key,
users can get all possible completions for the current word. This behavior is
controllable from Ada, where your application can provide the list of
completions.

Finally, readline comes with support for history. By using the :kbd:`up` and
:kbd:`down` keys, the user can navigate the commands that were previously
typed. It is also possible to preserve the history across sessions.

See the package `GNATCOLL.Readline` for more documentation on the API.

This package is always usable, whether or not the system supports readline.
When readline is not available, the package falls back to a `Text_IO` based
implementation (still providing input, although none of the advanced features
described above will work).

