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

signature TELESCOPE =
sig
  type 'a telescope

  structure Label : ORDERED
  type label = Label.t

  (* smart constructors *)
  val empty : 'a telescope
  val snoc : 'a telescope -> label -> 'a -> 'a telescope
  val cons : label -> 'a -> 'a telescope -> 'a telescope

  val singleton : label -> 'a -> 'a telescope

  val append : 'a telescope * 'a telescope -> 'a telescope

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

signature SHOW_TELESCOPE =
sig
  structure T : TELESCOPE

  val toString : ('a -> string) -> 'a T.telescope -> string
end

signature COMPARE_TELESCOPE =
sig
  structure E : ORDERED
  structure T : TELESCOPE

  (* exact equality, does not respect alpha equivalence *)
  val eq : E.t T.telescope * E.t T.telescope -> bool
  val subtelescope : E.t T.telescope * E.t T.telescope -> bool
end

signature UNIFY_TELESCOPE =
sig
  structure T : TELESCOPE

  type term
  type ren

  (* alpha-equivalence of telescopes; throws [UnificationFailed]
   * if the telescopes do not unify. When [unify (t1, t2)] ==> [rho],
   * this means that [rho*t1] == [t2]. *)
  val unifyEq : term T.telescope * term T.telescope -> ren

  (* alpha-subtelescoping *)
  val unifySub : term T.telescope * term T.telescope -> ren

  (* total versions of [unifyEq], [unifySub] *)
  val unifyEqOpt : term T.telescope * term T.telescope -> ren option
  val unifySubOpt : term T.telescope * term T.telescope -> ren option

  exception UnificationFailed
end

signature SEARCH_TELESCOPE =
sig
  structure T : TELESCOPE
  val search : 'a T.telescope -> ('a -> bool) -> (T.label * 'a) option
end

signature TELESCOPE_NOTATION =
sig
  type 'a telescope
  type label

  val >: : 'a telescope * (label * 'a) -> 'a telescope
end

