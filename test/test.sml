functor Test (T : TELESCOPE where type Label.t = string) =
struct
  exception Test

  structure Notation = TelescopeNotation (T)
  structure Compare = CompareTelescope (structure T = T and E = IntOrdered)
  structure Show = ShowTelescope (structure T = T fun labelToString x = x)
  open Notation Show Compare T infix >:

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
      val tele = empty >: ("1", 1) >: ("2", 2) >: ("3",3)
    in
      assert "refl" @@ eq (tele, tele);
      assert "foldr" @@ T.foldr op:: [] tele = [1,2,3];
      assert "foldl" @@ T.foldl op:: [] tele = [3,2,1]
    end

end

structure Test = Test (Telescope (StringOrdered))
