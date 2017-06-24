structure ListDeque :> DEQUE_MINI =
struct
  type 'a t = 'a list * 'a list
  
  val empty = (nil, nil)

  fun isEmpty (l1, l2) = List.null l1 andalso List.null l2

  fun singleton a = ([a], [])

  fun cons (a, (l1, l2)) = (a :: l1, l2)

  fun snoc ((l1, l2), a) = (l1, a :: l2)

  (* O(1) *)
  fun fromList l = (l, [])

  fun toList (l1, []) = l1 (* O(1)-time after fromList *)
    | toList (l1, l2) = l1 @ List.rev l2

  fun append ((l1, l2), (l1', l2'))
    = (l1, List.concat [l2', List.rev l1', l2])

  fun filter f d = fromList (List.filter f (toList d))

  fun foldl f b (l1, l2) = List.foldr f (List.foldl f b l1) l2
  fun foldr f b (l1, l2) = List.foldr f (List.foldl f b l2) l1

  datatype 'a consview = EMPTY | CONS of ('a * 'a t)

  fun showcons (x :: l1, l2) = CONS (x, (l1, l2))
    | showcons ([], []) = EMPTY
    | showcons ([], l2) = showcons (List.rev l2, [])
end
