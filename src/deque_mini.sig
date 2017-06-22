signature DEQUE_MINI =
sig
  type 'a t

  val empty : 'a t
  val singleton : 'a -> 'a t
  val cons : 'a * 'a t -> 'a t
  val snoc : 'a t * 'a -> 'a t
  val fromList : 'a list -> 'a t
  val toList : 'a t -> 'a list
  val append : 'a t * 'a t -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t

  datatype 'a consview = NIL | CONS of ('a * 'a t)
  val showcons : 'a t -> 'a consview

  (* this makes sure that the following toList and
   * showcons are O(1). *)
  val force : 'a t -> 'a t
end
