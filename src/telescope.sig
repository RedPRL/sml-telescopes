signature SNOC_VIEW =
sig
  type 'a telescope
  type label

  datatype ('a, 'r) view =
       EMPTY
     | SNOC of 'r * label * 'a

  val out : 'a telescope -> ('a, 'a telescope) view
  val into : ('a, 'a telescope) view -> 'a telescope
end

signature CONS_VIEW =
sig
  type 'a telescope
  type label

  datatype ('a, 'r) view =
       EMPTY
     | CONS of label * 'a * 'r

  val out : 'a telescope -> ('a, 'a telescope) view
  val outAfter : 'a telescope -> label -> ('a, 'a telescope) view
  val into : ('a, 'a telescope) view -> 'a telescope
end

signature TELESCOPE =
sig
  type 'a telescope

  structure Label : ORDERED
  type label = Label.t

  (* smart constructors *)
  val empty : 'a telescope
  val snoc : 'a telescope -> label -> 'a -> 'a telescope
  val cons : label -> 'a -> 'a telescope -> 'a telescope

  val append : 'a telescope * 'a telescope -> 'a telescope

  (* lookup and search *)
  val lookup : 'a telescope -> label -> 'a
  val find : 'a telescope -> label -> 'a option
  val search : 'a telescope -> ('a -> bool) -> (label * 'a) option

  (* manipulation *)
  val map : ('a -> 'b) -> 'a telescope -> 'b telescope
  val modify : label -> ('a -> 'a) -> 'a telescope -> 'a telescope
  val modifyAfter : label -> ('a -> 'a) -> 'a telescope -> 'a telescope
  val remove : label -> 'a telescope -> 'a telescope

  val interposeAfter : 'a telescope -> label * 'a telescope -> 'a telescope
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a telescope -> 'b
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a telescope -> 'b

  (* These views may be used to lazily walk along a telescope *)
  structure SnocView : SNOC_VIEW
    where type 'a telescope = 'a telescope
    where type label = label

  structure ConsView : CONS_VIEW
    where type 'a telescope = 'a telescope
    where type label = label
end

signature TELESCOPE_COMPARE =
sig
  structure E : ORDERED
  structure T : TELESCOPE

  val eq : E.t T.telescope * E.t T.telescope -> bool
  val subtelescope : E.t T.telescope * E.t T.telescope -> bool
end

signature TELESCOPE_NOTATION =
sig
  type 'a telescope
  type label

  val >: : 'a telescope * (label * 'a) -> 'a telescope
end

