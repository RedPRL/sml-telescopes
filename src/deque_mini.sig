signature DEQUE_MINI =
sig
  type 'a t

  val empty : 'a t
  val isEmpty : 'a t -> bool
  val singleton : 'a -> 'a t
  val cons : 'a * 'a t -> 'a t
  val snoc : 'a t * 'a -> 'a t
  val fromList : 'a list -> 'a t
  val toList : 'a t -> 'a list
  val append : 'a t * 'a t -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val foldl : (('a * 'b) -> 'b) -> 'b -> 'a t -> 'b
  val foldr : (('a * 'b) -> 'b) -> 'b -> 'a t -> 'b

  datatype 'a consview = EMPTY | CONS of ('a * 'a t)
  val showcons : 'a t -> 'a consview
end
