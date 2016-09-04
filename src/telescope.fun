functor MergeDict (Dict : DICT) =
struct
  datatype merge_policy = DISJOINT | OVERWRITE

  exception DictsNotDisjoint

  fun mergeDict policy (d1, d2) =
    Dict.foldl (fn (a, b, d3) =>
      case policy of
           OVERWRITE => Dict.insert d3 a b
         | DISJOINT =>
             (case Dict.find d3 a of
                  NONE => Dict.insert d3 a b
                | SOME _ => raise DictsNotDisjoint)
    ) d2 d1
end

functor Telescope (L : ORDERED) :> TELESCOPE where type Label.t = L.t =
struct
  structure Label = L
  structure D = SplayDict (structure Key = L)
  structure MergeDict = MergeDict (D)

  type label = L.t

  exception Absent

  structure Internal =
  struct
    type 'a telescope = L.t list * 'a D.dict

    val empty = ([], D.empty)

    fun snoc (list, dict) lbl x =
      if D.member dict lbl then
        raise MergeDict.DictsNotDisjoint
      else
        (lbl :: list, D.insert dict lbl x)

    fun cons lbl x (list, dict) =
      if D.member dict lbl then
        raise MergeDict.DictsNotDisjoint
      else
        (list @ [lbl], D.insert dict lbl x)

    fun append (list1, dict1) (list2, dict2) =
      let
        val dict = MergeDict.mergeDict MergeDict.DISJOINT (dict1, dict2)
      in
        (list2 @ list1, dict)
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
        fun go [] dict = dict
          | go (l :: ls) dict =
              if L.eq (l, lbl) then
                dict
              else
                let
                  val a = D.lookup dict l
                  val a' = f a
                in
                  go ls (D.insert dict l a')
                end

      in
        (list, go list dict)
      end

    fun remove lbl (list, dict) =
      (List.filter (fn l => not (L.eq (l, lbl))) list,
       D.remove dict lbl)

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
        val dict' = MergeDict.mergeDict MergeDict.DISJOINT (D.remove dict x, dictx)
        val (xs, ys) = splitList x list
      in
        (List.rev xs @ listx @ ys, dict')
      end

    fun truncateFrom (ys, dict) y =
      if D.member dict y then
        let
          val (xs, zs) = splitList y ys
        in
          (zs, List.foldl (fn (x, dict') => D.remove dict' x) dict xs)
        end
      else
        (ys, dict)

    fun dropUntil (ys, dict) y =
      if D.member dict y then
        let
          val (xs, zs) = splitList y ys
        in
          (xs, List.foldl (fn (z, dict') => D.remove dict' z) dict zs)
        end
      else
        (ys, dict)

    fun foldr alg z (list, dict) =
      List.foldl (fn (x, b) => alg (D.lookup dict x, b)) z list

    fun foldl alg z (list, dict) =
      List.foldr (fn (x, b) => alg (D.lookup dict x, b)) z list

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

      val out =
        fn ([], _) => EMPTY
         | (xs as _ ::_, dict) =>
             let
               val x = List.last xs
               val a = D.lookup dict x
             in
               CONS (x, a, (List.take (xs, List.length xs - 1), D.remove dict x))
             end

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

      val out =
        fn ([], _) => EMPTY
         | (x :: xs, dict) => SNOC ((xs, D.remove dict x), x, D.lookup dict x)

      val into =
        fn EMPTY => empty
         | SNOC (r, x, a) => snoc r x a
    end
  end

  open Internal

  fun singleton lbl x =
    cons lbl x empty

  fun interposeAfter t x t' =
    splice t x (cons x (lookup t x) t')

end

functor SearchTelescope (T : TELESCOPE) : SEARCH_TELESCOPE =
struct
  structure T = T
  open T.SnocView

  fun search tel phi =
    let
      val rec go =
        fn EMPTY => NONE
         | SNOC (tele', lbl, a) =>
             if phi a then
               SOME (lbl, a)
             else
               go (out tele')
    in
      go (out tel)
    end
end

functor ShowTelescope
  (structure T : TELESCOPE
   val labelToString : T.label -> string) : SHOW_TELESCOPE =
struct
  structure T = T
  open T.ConsView

  fun toString pretty =
    let
      fun go r =
        fn EMPTY => r
         | CONS (lbl, a, tele') =>
            go (r ^ ", " ^ labelToString lbl ^ " : " ^ pretty a) (out tele')
    in
      go "\194\183" o out
    end
end

functor CompareTelescope
  (structure T : TELESCOPE
   structure E : ORDERED) : COMPARE_TELESCOPE =
struct
  structure T = T and E = E
  local
    open T.SnocView
  in
    fun subtelescope (t1, t2) =
      let
        fun go EMPTY = true
          | go (SNOC (t1', lbl, a)) =
              case T.find t2 lbl of
                   NONE => false
                 | SOME a' => E.eq (a, a') andalso go (out t1')
      in
        go (out t1)
      end

    fun eq (t1, t2) =
      subtelescope (t1, t2)
        andalso subtelescope (t2, t1)
  end
end

functor UnifyTelescope
  (structure T : TELESCOPE
   type term
   structure Ren : DICT where type key = T.label
   val unifyTerm : term * term -> T.label Ren.dict option
   val rename : T.label Ren.dict -> term -> term) : UNIFY_TELESCOPE =
struct
  structure T = T
  type term = term
  type ren = T.label Ren.dict

  exception UnificationFailed

  open T.ConsView

  fun renUnion rho1 rho2 =
    Ren.union rho1 rho2 (fn (_, x, y) =>
      if T.Label.eq (x, y) then
        x
      else
        raise UnificationFailed)

  fun unifyEq (t1, t2) =
    let
      fun go rho =
        fn (EMPTY, EMPTY) => rho
         | (CONS (l1, a1, t1'), CONS (l2, a2, t2')) =>
             (case unifyTerm (rename rho a1, a2) of
                 SOME rho' =>
                   go (renUnion (Ren.insert rho l1 l2) rho') (out t1', out t2')
               | NONE => raise UnificationFailed)
         | _ => raise UnificationFailed
    in
      go Ren.empty (out t1, out t2)
    end

  fun unifySub (t1, t2) =
    let
      fun go rho =
        fn (CONS (l1, a1, t1'), CONS (l2, a2, t2')) =>
             let
               val rho' = Ren.insert rho l1 l2
             in
               case unifyTerm (rename rho a1, a2) of
                   SOME rho'' => go (renUnion rho' rho'') (out t1', out t2')
                 | NONE => go rho' (CONS (l1, a1, t1'), out t2')
             end
         | (EMPTY, _) => rho
         | (_, EMPTY) => raise UnificationFailed
    in
      go Ren.empty (out t1, out t2)
    end

  fun unifyEqOpt (t1, t2) =
    SOME (unifyEq (t1, t2) )
    handle UnificationFailed => NONE
         | e => raise e

  fun unifySubOpt (t1, t2) =
    SOME (unifySub (t1, t2) )
    handle UnificationFailed => NONE
         | e => raise e

end

functor TelescopeNotation (T : TELESCOPE) : TELESCOPE_NOTATION =
struct
  open T

  fun >: (tele, (l, a)) = snoc tele l a
end
