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

  fun interposeAfter (TEL {first,last,preds,nexts,vals}) (lbl, TEL tele) = TEL
    {first = first,
     last = case SOME (Dict.lookup nexts lbl) handle _ => NONE of
                 NONE => #last tele
               | SOME lbl' => last,
     preds =
       let
         val preds' = Dict.insert preds (#first tele) lbl
         val preds'' =
           case SOME (Dict.lookup nexts lbl) handle _ => NONE of
                NONE => preds'
              | SOME lblpst => Dict.insert preds' lblpst (#last tele)
       in
         MergeDict.mergeDict MergeDict.DISJOINT (#preds tele, preds'')
       end,
     nexts =
       let
         val nexts' = Dict.insert nexts lbl (#first tele)
         val nexts'' =
           case SOME (Dict.lookup nexts lbl) handle _ => NONE of
                NONE => nexts'
              | SOME lblpst => Dict.insert nexts' (#last tele) lblpst
       in
         MergeDict.mergeDict MergeDict.DISJOINT (#nexts tele, nexts'')
       end,
     vals = MergeDict.mergeDict MergeDict.DISJOINT (vals, #vals tele)}
    | interposeAfter tele (lbl, NIL) = tele
    | interposeAfter NIL (lbl, tele) = tele

  fun append (NIL, t) = t
    | append (t as TEL {last,...}, t') =
        interposeAfter t (last, t')

  fun modify lbl f =
    fn NIL => NIL
     | TEL {first,last,preds,nexts,vals} =>
         let
           val a = Dict.lookup vals lbl
           val vals' = Dict.insert vals lbl (f a)
         in
           TEL
             {first = first,
              last = last,
              preds = preds,
              nexts = nexts,
              vals = vals'}
         end

  fun lookup (TEL {vals,...} : 'a telescope) lbl = Dict.lookup vals lbl
    | lookup NIL lbl = raise Fail "Lookup empty"

  fun find (TEL {vals,...} : 'a telescope) lbl = Dict.find vals lbl
    | find _ _ = NONE

  val empty = NIL

  fun singleton (lbl, a) =
    TEL
    {first = lbl,
     last = lbl,
     nexts = Dict.empty,
     preds = Dict.empty,
     vals = Dict.insert Dict.empty lbl a}

  fun cons lbl a tele = interposeAfter (singleton (lbl, a)) (lbl, tele)

  fun snoc (TEL tele) lbl a = interposeAfter (TEL tele) (#last tele, singleton (lbl, a))
    | snoc NIL lbl a = singleton (lbl, a)

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
      | out (TEL {first,last,preds,nexts,vals}) =
          let
            val tail =
              case SOME (Dict.lookup preds last) handle _ => NONE of
                   NONE => NIL
                 | SOME pred =>
                     TEL
                       {first = first,
                        last = pred,
                        preds = preds,
                        nexts = nexts,
                        vals = vals}
          in
            SNOC (tail, last, Dict.lookup vals last)
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
      | out (TEL {first,last,preds,nexts,vals}) =
          let
            val tail =
              case SOME (Dict.lookup nexts first) handle _ => NONE of
                   NONE => NIL
                 | SOME next =>
                     TEL
                      {first = next,
                       last = last,
                       preds = preds,
                       nexts = nexts,
                       vals = vals}
          in
            CONS (first, Dict.lookup vals first, tail)
          end

    fun outAfter NIL lbl = EMPTY
      | outAfter (TEL {first,last,preds,nexts,vals}) lbl =
         out (TEL
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
                    go (Dict.insert D lbl (f (Dict.lookup D lbl))) (out tele)
           in
              TEL
                {first = first,
                 last = last,
                 preds = preds,
                 nexts = nexts,
                 vals = go vals (out (TEL tele))}
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
  end

  local
    open SnocView
  in
    fun search (tele : 'a telescope) phi =
      let
        fun go EMPTY = NONE
          | go (SNOC (tele', lbl, a)) =
              if phi a then
                SOME (lbl, a)
              else
                go (out tele')
      in
        go (out tele)
      end

    fun subtelescope test (t1, t2) =
      let
        fun go EMPTY = true
          | go (SNOC (t1', lbl, a)) =
              case find t2 lbl of
                   NONE => false
                 | SOME a' => test (a, a') andalso go (out t1')
      in
        go (out t1)
      end

    fun eq test (t1, t2) =
      subtelescope test (t1, t2)
        andalso subtelescope test (t2, t1)
  end
end

functor ShowTelescope
  (structure T : TELESCOPE
   val labelToString : T.label -> string) :>
sig
  val toString : ('a -> string) -> 'a T.telescope -> string
end =
struct
  open T.ConsView

  fun toString pretty tele =
    let
      fun go EMPTY r = r
        | go (CONS (lbl, a, tele')) r =
            go (out tele') (r ^ ", " ^ labelToString lbl ^ " : " ^ pretty a)
    in
      go (out tele) "\194\183"
    end
end

functor TelescopeNotation (T : TELESCOPE) : TELESCOPE_NOTATION =
struct
  open T

  fun >: (tele, (l, a)) = snoc tele l a
end
