functor Telescope (L : TELESCOPE_LABEL) :> TELESCOPE where type Label.t = L.t =
struct
  structure Label = L
  structure D = SplayDict (structure Key = L)
  structure LD = ListDeque

  type label = L.t

  exception Absent
  exception Duplicate of label

  structure Internal =
  struct
    type 'a telescope = L.t LD.t * 'a D.dict

    fun isEmpty (list, _) = LD.isEmpty list

    val empty = (LD.empty, D.empty)

    fun snoc (list, dict) lbl x =
      if D.member dict lbl then
        raise Duplicate lbl
      else
        (LD.snoc (list, lbl), D.insert dict lbl x)

    fun cons lbl x (list, dict) =
      if D.member dict lbl then
        raise Duplicate lbl
      else
        (LD.cons (lbl, list), D.insert dict lbl x)

    fun append (list1, dict1) (list2, dict2) =
      let
        val dict = D.union dict1 dict2 (fn (l, _, _) => raise Duplicate l)
      in
        (LD.append (list1, list2), dict)
      end

    fun lookup (list, dict) lbl =
      D.lookup dict lbl handle _ => raise Absent

    fun find (list, dict) lbl =
      D.find dict lbl

    fun map f (list, dict) =
      (list, D.map f dict)

    fun modify lbl f (list, dict) =
      let
        val (_, _, dict') = D.operate dict lbl (fn _ => raise Absent) f
      in
        (list, dict')
      end

    fun modifyAfter lbl f (list, dict) =
      let
        val list = LD.toList list
        fun dropPrefix [] = []
          | dropPrefix (l :: ls) =
              if L.eq (l, lbl) then ls else dropPrefix ls
        val suffix = dropPrefix list
        val updatedDict = List.foldl
              (fn (l, d) => #3 (D.operate' d l (fn _ => NONE) (SOME o f)))
              dict suffix
      in
        (LD.fromList list, updatedDict)
      end

    fun remove lbl (list, dict) =
      (LD.filter (fn l => not (L.eq (l, lbl))) list,
       D.remove dict lbl)

    (* this intends to returns a tuple (xs, ys)
     * such that revAppend xs (y :: ys) is the input. *)
    fun splitList x =
      let
        fun go xs [] = (xs, [])
          | go xs (y :: ys) =
              if L.eq (y, x) then
                (xs, ys)
              else
                go (y :: xs) ys
      in
        go []
      end

    fun splice (list, dict) x (listx, dictx) =
      let
        val dict' = D.union (D.remove dict x) dictx (fn (l, _, _) => raise Duplicate l)
        val (xs, ys) = splitList x (LD.toList list)
      in
        (LD.fromList (List.concat [List.rev xs, (LD.toList listx), ys]), dict')
      end

    fun truncateFrom (ys, dict) y =
      if D.member dict y then
        let
          val (xs, zs) = splitList y (LD.toList ys)
        in
          (LD.fromList (List.rev xs), List.foldl (fn (z, dict') => D.remove dict' z) dict (y :: zs))
        end
      else
        (ys, dict)

    fun dropUntil (ys, dict) y =
      if D.member dict y then
        let
          val (xs, zs) = splitList y (LD.toList ys)
        in
          (LD.fromList zs, List.foldl (fn (x, dict') => D.remove dict' x) dict xs)
        end
      else
        (ys, dict)

    fun foldl alg z (list, dict) =
      LD.foldl (fn (x, b) => alg (x, D.lookup dict x, b)) z list

    fun foldr alg z (list, dict) =
      LD.foldr (fn (x, b) => alg (x, D.lookup dict x, b)) z list

    structure ConsView =
    struct
      type 'a telescope = 'a telescope
      type label = label

      datatype ('a, 'r) view =
          EMPTY
        | CONS of label * 'a * 'r

      val into =
        fn EMPTY => empty
         | CONS (lbl, a, r) => cons lbl a r

      fun out (list, dict) =
        case LD.showcons list of
             LD.EMPTY => EMPTY
           | LD.CONS (x, xs) =>
               CONS (x, D.lookup dict x, (xs, D.remove dict x))

      fun outAfter x t =
        out (dropUntil t x)
    end

    structure SnocView =
    struct
      type 'a telescope = 'a telescope
      type label = label

      datatype ('a, 'r) view =
           EMPTY
         | SNOC of 'r * label * 'a

      val into =
        fn EMPTY => empty
         | SNOC (r, x, a) => snoc r x a

      (* favonia: this is inefficient by design.
       * We decided to sacrifice SnocView for ConsView (and simplicity). *)
      val out' =
        fn ([], _) => EMPTY
         | (l as _ :: _ , dict) =>
             let
               fun showsnoc (_, []) = raise List.Empty
                 | showsnoc (xs, [y]) = (List.rev xs, y)
                 | showsnoc (xs, y :: z :: zs) =
                     showsnoc (y :: xs, z :: zs)
                val (xs, x) = showsnoc ([], l)
             in
               SNOC ((LD.fromList xs, D.remove dict x), x, D.lookup dict x)
             end
      fun out (list, dict) = out' (LD.toList list, dict)
    end
  end

  open Internal

  fun singleton lbl x =
    cons lbl x empty

  fun interposeAfter t x t' =
    splice t x (cons x (lookup t x) t')

  local
    open ConsView
  in
    fun subtelescope f (t1, t2) =
      case out t1 of
        EMPTY => true
      | CONS (lbl, a1, t1) =>
          case find t2 lbl of
            NONE => false
          | SOME a2 => f (a1, a2)
              andalso subtelescope f (t1, dropUntil t2 lbl)

    fun eq f (t1, t2) =
      case (out t1, out t2) of
        (EMPTY, EMPTY) => true
      | (CONS (lbl1, a1, t1), CONS (lbl2, a2, t2)) =>
          L.eq (lbl1, lbl2) andalso f (a1, a2) andalso eq f (t1, t2)
      | _ => false
  end

end

functor TelescopeUtil (T : TELESCOPE) : TELESCOPE_UTIL =
struct
  open T

  fun search tel phi =
    let
      open ConsView
      val rec go =
        fn EMPTY => NONE
         | CONS (lbl, a, tel) =>
             if phi a then
               SOME (lbl, a)
             else
               go (out tel)
    in
      go (out tel)
    end

  fun toString pretty =
    let
      open ConsView
      fun go r =
        fn EMPTY => r
         | CONS (lbl, a, tele') =>
            go (r ^ ", " ^ T.Label.toString lbl ^ " : " ^ pretty a) (out tele')
    in
      go "\194\183" o out
    end
end

functor TelescopeNotation (T : TELESCOPE) : TELESCOPE_NOTATION =
struct
  open T

  fun >: (tele, (l, a)) = snoc tele l a
end
