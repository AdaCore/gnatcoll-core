with Interfaces.C; use Interfaces.C;

procedure Main is

   function Foo (X : int) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "foo";

   V : constant int := Foo (5);

begin
   null;
end Main;
