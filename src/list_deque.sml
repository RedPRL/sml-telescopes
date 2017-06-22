structure ListDeque :> DEQUE_MINI =
struct
  type 'a t = 'a list * 'a list
  
  val empty = (nil, nil)

  fun singleton a = ([a], [])

  fun cons (a, (l1, l2)) = (a :: l1, l2)

  fun snoc ((l1, l2), a) = (l1, a :: l2)

  fun fromList l = (l, [])

  fun toList (l1, []) = l1 (* O(1)-time after fromList and force *)
    | toList (l1, l2) = l1 @ List.rev l2

  fun append ((l1, l2), (l1', l2'))
    = (l1, List.concat [l2', List.rev l1', l2])

  fun filter f d = fromList (List.filter f (toList d))

  datatype 'a consview = NIL | CONS of ('a * 'a t)

  fun showcons (x :: l1, l2) = CONS (x, (l1, l2))
    | showcons ([], []) = NIL
    | showcons ([], l2) = showcons (List.rev l2, [])

  fun force (l1, l2) = (l1 @ List.rev l2, [])
end
