.. _Searching_strings:

**********************************
**Boyer-Moore**: Searching strings
**********************************

.. highlight:: ada
.. index:: Boyer-Moore
.. index:: search

Although the Ada standard provides a number of string-searching subprograms
(most notably in the `Ada.Strings.Fixed`, `Ada.Strings.Unbounded`
and `Ada.Strings.Bounded` packages through the `Index` functions),
these subprograms do not in general provide the most efficient algorithms
for searching strings.

The package **GNATCOLL.Boyer_Moore** provides one such optimize algorithm,
although there exists several others which might be more efficient depending
on the pattern.

It deals with string searching, and does not handle regular expressions for
instance.

This algorithm needs to preprocess its key (the searched string), but does
not need to perform any specific analysis of the string to be searched.
Its execution time can be sub-linear: it doesn't need to actually check
every character of the string to be searched, and will skip over some of
them. The worst case for this algorithm has been proved to need approximately
3 * N comparisons, hence the algorithm has a complexity of O(n).

The longer the key, the faster the algorithm in general, since that provides
more context as to how many characters can be skipped when a non-matching
character is found..

We will not go into the details of the algorithm, although a general
description follows: when the pattern is being preprocessed, Boyer-Moore
computes how many characters can be skipped if an incorrect match is
found at that point, depending on which character was read.
In addition, this algorithm tries to match the key starting from its end,
which in general provides a greater number of characters to skip.

For instance, if you are looking for "ABC" in the string "ABDEFG" at the
first position, the algorithm will compare "C" and "D". Since "D" does not
appear in the key "ABC", it knows that it can immediately skip 3 characters
and start the search after "D".

Using this package is extremely easy, and it has only a limited API::

  declare
    Str : constant String := "ABDEABCFGABC";
    Key : Pattern;
    Index : Integer;
  begin
    Compile (Key, "ABC");
    Index := Search (Key, Str);
  end

`Search` will either return -1 when the pattern did not match, or
the index of the first match in the string. In the example above, it
will return 5.

If you want to find the next match, you have to pass a substring to
search, as in::

    Index := Search (Key, Str (6 .. Str'Last));

