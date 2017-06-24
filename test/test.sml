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
      val tele1234 = empty >: ("1",1) >: ("2", 2) >: ("3",3) >: ("4",4)
      val tele12 = empty >: ("1",1) >: ("2",2)
      val tele21 = empty >: ("2",2) >: ("1",1)
      val tele34 = empty >: ("3",3) >: ("4",4)
    in
      assert "refl" @@ eq op= (tele1234, tele1234);
      assert "eq/reorder" @@ not (eq op= (tele12, tele21));
      assert "foldr" @@ T.foldr (fn (_, x, r) => x :: r) [] tele1234 = [1,2,3,4];
      assert "foldl" @@ T.foldl (fn (_, x, r) => x :: r) [] tele1234 = [4,3,2,1];
      assert "truncateFrom" @@ eq op= (truncateFrom tele1234 "3", tele12);
      assert "truncateFrom/not-a-key" @@ eq op= (truncateFrom tele1234 "not-a-key", tele1234);
      assert "dropUntil" @@ eq op= (dropUntil tele1234 "2", tele34);
      assert "dropUntil/not-a-key" @@ eq op= (dropUntil tele1234 "not-a-key", tele1234)
    end

end

structure Test = Test (Telescope (open StringOrdered fun toString x = x))
