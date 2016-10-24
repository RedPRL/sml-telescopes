functor Test (T : TELESCOPE where type Label.t = string) =
struct
  exception Test

  structure T = TelescopeUtil (T)
  structure Notation = TelescopeNotation (T)
  open Notation T infix >:

  fun @@ (f, x) = f x
  infixr @@

  fun printTele t =
    print ("\n" ^ toString Int.toString t ^ "\n\n")

  fun assert msg b =
    if b then
      print ("Success: " ^ msg ^ "\n")
    else
      print ("Failure: " ^ msg ^ "\n")

  val _ =
    let
      val tele = empty >: ("1", 1) >: ("2", 2) >: ("3",3) >: ("4",4)
      val tele' = empty >: ("1", 1) >: ("2",2)
    in
      assert "refl" @@ eq op= (tele, tele);
      assert "foldr" @@ T.foldr (fn (_, x, r) => x :: r) [] tele = [1,2,3,4];
      assert "foldl" @@ T.foldl (fn (_, x, r) => x :: r) [] tele = [4,3,2,1];
      assert "truncateFrom" @@ eq op= (truncateFrom tele "3", tele');
      assert "truncateFrom/not-a-key" @@ eq op= (truncateFrom tele "not-a-key", tele)
    end

end

structure Test = Test (Telescope (open StringOrdered fun toString x = x))
