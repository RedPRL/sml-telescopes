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
  type label = L.t
  structure Label = L

  structure Dict = SplayDict(structure Key = L)
  structure MergeDict = MergeDict (Dict)

  datatype 'a telescope =
      TEL of
        {first : L.t,
         last : L.t,
         preds : L.t Dict.dict,
         nexts : L.t Dict.dict,
         vals : 'a Dict.dict}
    | NIL

  fun foldr f init =
    fn TEL r => Dict.foldr (fn (_,a,b) => f (a,b)) init (#vals r)
     | NIL => init

  fun foldl f init =
    fn TEL r => Dict.foldl (fn (_,a,b) => f (a,b)) init (#vals r)
     | NIL => init

  fun interposeAfter (TEL {first,last,preds,nexts,vals}) lbl (TEL tele) = TEL
    {first = first,
     last = case Dict.find nexts lbl of
                 NONE => #last tele
               | SOME lbl' => last,
     preds =
       let
         val preds' = Dict.insert preds (#first tele) lbl
         val preds'' =
           case Dict.find nexts lbl of
                NONE => preds'
              | SOME lblpst => Dict.insert preds' lblpst (#last tele)
       in
         MergeDict.mergeDict MergeDict.DISJOINT (#preds tele, preds'')
       end,
     nexts =
       let
         val nexts' = Dict.insert nexts lbl (#first tele)
         val nexts'' =
           case Dict.find nexts lbl of
                NONE => nexts'
              | SOME lblpst => Dict.insert nexts' (#last tele) lblpst
       in
         MergeDict.mergeDict MergeDict.DISJOINT (#nexts tele, nexts'')
       end,
     vals = MergeDict.mergeDict MergeDict.DISJOINT (vals, #vals tele)}
    | interposeAfter tele lbl NIL = tele
    | interposeAfter NIL lbl tele = tele

  fun append (NIL, t) = t
    | append (t as TEL {last,...}, t') =
        interposeAfter t last t'

  exception Absent

  fun lookup (TEL {vals,...}) lbl = (Dict.lookup vals lbl handle _ => raise Absent)
    | lookup NIL lbl = raise Absent

  fun find (TEL {vals,...}) lbl = Dict.find vals lbl
    | find _ _ = NONE

  fun modify lbl f =
    fn NIL => NIL
     | tel as TEL {first,last,preds,nexts,vals} =>
         let
           val a = lookup tel lbl
           val vals' = Dict.insert vals lbl (f a)
         in
           TEL
             {first = first,
              last = last,
              preds = preds,
              nexts = nexts,
              vals = vals'}
         end


  val empty = NIL

  fun singleton lbl a =
    TEL
      {first = lbl,
       last = lbl,
       nexts = Dict.empty,
       preds = Dict.empty,
       vals = Dict.insert Dict.empty lbl a}

  fun cons lbl a tele =
    interposeAfter (singleton lbl a) lbl tele

  fun snoc (TEL tele) lbl = interposeAfter (TEL tele) (#last tele) o singleton lbl
    | snoc NIL lbl = singleton lbl

  fun map f =
    fn NIL => NIL
     | TEL {first,last,preds,nexts,vals} =>
         TEL
           {first = first,
            last = last,
            preds = preds,
            nexts = nexts,
            vals = Dict.map f vals}

  structure SnocView =
  struct
    type 'a telescope = 'a telescope
    type label = label

    datatype ('a, 'r) view =
        EMPTY
      | SNOC of 'r * label * 'a

    fun out NIL = EMPTY
      | out (tel as TEL {first,last,preds,nexts,vals}) =
          let
            val tail =
              case Dict.find preds last of
                   NONE => NIL
                 | SOME pred =>
                     TEL
                       {first = first,
                        last = pred,
                        preds = preds,
                        nexts = nexts,
                        vals = vals}
          in
            SNOC (tail, last, lookup tel last)
          end

    fun into EMPTY = empty
      | into (SNOC (tel, lbl, a)) = snoc tel lbl a
  end

  structure ConsView =
  struct
    type 'a telescope = 'a telescope
    type label = label

    datatype ('a, 'r) view =
        EMPTY
      | CONS of label * 'a * 'r

    fun out NIL = EMPTY
      | out (tel as TEL {first,last,preds,nexts,vals}) =
          let
            val tail =
              case Dict.find nexts first of
                   NONE => NIL
                 | SOME next =>
                     TEL
                      {first = next,
                       last = last,
                       preds = preds,
                       nexts = nexts,
                       vals = vals}
          in
            CONS (first, lookup tel first, tail)
          end

    fun outAfter lbl =
      fn NIL => EMPTY
       | TEL {first,last,preds,nexts,vals} =>
         out
          (TEL
            {first = lbl,
             last = last,
             preds = preds,
             nexts = nexts,
             vals = vals})

    fun into EMPTY = empty
      | into (CONS (lbl, a, tele)) = cons lbl a tele
  end

  local
    open ConsView
  in
    fun modifyAfter lbl f =
      fn NIL => NIL
       | TEL (tele as {first,last,preds,nexts,vals}) =>
           let
             fun go D =
               fn EMPTY => D
                | CONS (lbl, a, tele) =>
                    go (Dict.insert D lbl (f (Dict.lookup D lbl handle _ => raise Absent))) (out tele)
           in
              TEL
                {first = first,
                 last = last,
                 preds = preds,
                 nexts = nexts,
                 vals = go vals (outAfter lbl (TEL tele))}
           end

    fun remove lbl tele =
      let
        val rec go =
          fn EMPTY => empty
           | CONS (lbl', a, tele') =>
              if Label.eq (lbl, lbl') then
                go (out tele')
              else
                cons lbl' a (go (out tele'))
      in
        go (out tele)
      end

    fun splice t1 lbl =
      remove lbl o interposeAfter t1 lbl

  end
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
