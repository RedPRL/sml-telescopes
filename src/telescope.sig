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
  val outAfter : label -> 'a telescope -> ('a, 'a telescope) view
  val into : ('a, 'a telescope) view -> 'a telescope
end

signature TELESCOPE_LABEL =
sig
  include ORDERED
  val toString : t -> string
end

signature TELESCOPE =
sig
  type 'a telescope

  structure Label : TELESCOPE_LABEL
  type label = Label.t

  val isEmpty : 'a telescope -> bool

  (* smart constructors *)
  val empty : 'a telescope
  val snoc : 'a telescope -> label -> 'a -> 'a telescope
  val cons : label -> 'a -> 'a telescope -> 'a telescope

  val singleton : label -> 'a -> 'a telescope

  val append : 'a telescope -> 'a telescope -> 'a telescope

  (* lookup *)
  val lookup : 'a telescope -> label -> 'a
  val find : 'a telescope -> label -> 'a option

  exception Absent

  (* manipulation *)
  val map : ('a -> 'b) -> 'a telescope -> 'b telescope
  val modify : label -> ('a -> 'a) -> 'a telescope -> 'a telescope
  val modifyAfter : label -> ('a -> 'a) -> 'a telescope -> 'a telescope
  val remove : label -> 'a telescope -> 'a telescope

  val splice : 'a telescope -> label -> 'a telescope -> 'a telescope
  val interposeAfter : 'a telescope -> label -> 'a telescope -> 'a telescope

  (* removes every element of the telescope, starting at the provided label *)
  val truncateFrom : 'a telescope -> label -> 'a telescope
  val dropUntil : 'a telescope -> label -> 'a telescope

  val foldr : (label * 'a * 'b -> 'b) -> 'b -> 'a telescope -> 'b
  val foldl : (label * 'a * 'b -> 'b) -> 'b -> 'a telescope -> 'b

  (* exact equality, not alpha equivalence *)
  val eq : ('a * 'a -> bool) -> 'a telescope * 'a telescope -> bool

  (* These views may be used to lazily walk along a telescope *)
  structure SnocView : SNOC_VIEW
    where type 'a telescope = 'a telescope
    where type label = label

  structure ConsView : CONS_VIEW
    where type 'a telescope = 'a telescope
    where type label = label
end

signature TELESCOPE_UTIL =
sig
  include TELESCOPE
  val toString : ('a -> string) -> 'a telescope -> string
  val search : 'a telescope -> ('a -> bool) -> (label * 'a) option
end

signature TELESCOPE_NOTATION =
sig
  type 'a telescope
  type label

  val >: : 'a telescope * (label * 'a) -> 'a telescope
end

