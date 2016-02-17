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

functor Telescope (L : LABEL) :> TELESCOPE where type Label.t = L.t =
struct
  type label = L.t
  structure Label = L

  structure Dict = SplayDict(structure Key = L)
  structure MergeDict = MergeDict (Dict)

  type 'a telescope =
    {first : L.t,
     last : L.t,
     preds : L.t Dict.dict,
     nexts : L.t Dict.dict,
     vals : 'a Dict.dict} option

  exception LabelExists

  fun foldr (f : ('a * 'b) -> 'b) (init : 'b) (t as SOME r : 'a telescope) : 'b =
    Dict.foldr (fn (_,a,b) => f (a,b)) init (#vals r)
    | foldr f init NONE = init

  fun foldl (f : ('a * 'b) -> 'b) (init : 'b) (t as SOME r : 'a telescope) : 'b =
    Dict.foldl (fn (_,a,b) => f (a,b)) init (#vals r)
    | foldl f init NONE = init

  fun interposeAfter (SOME {first,last,preds,nexts,vals}) (lbl, SOME tele) = SOME
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
    | interposeAfter tele (lbl, NONE) = tele
    | interposeAfter NONE (lbl, tele) = tele


  fun append (NONE, t) = t
    | append (t as SOME {last,...}, t') =
        interposeAfter t (last, t')

  fun modify NONE _ = NONE
    | modify (SOME {first,last,preds,nexts,vals}) (lbl, f) =
      let
        val a = Dict.lookup vals lbl
        val vals' = Dict.insert vals lbl (f a)
      in
        SOME
          {first = first,
           last = last,
           preds = preds,
           nexts = nexts,
           vals = vals'}
      end

  fun lookup (SOME {vals,...} : 'a telescope) lbl = Dict.lookup vals lbl
    | lookup NONE lbl = raise Fail "Lookup empty"

  fun find (SOME {vals,...} : 'a telescope) lbl = Dict.find vals lbl
    | find _ _ = NONE

  val empty = NONE

  fun singleton (lbl, a) =
    SOME
    {first = lbl,
     last = lbl,
     nexts = Dict.empty,
     preds = Dict.empty,
     vals = Dict.insert Dict.empty lbl a}

  fun cons (lbl, a) tele = interposeAfter (singleton (lbl, a)) (lbl, tele)

  fun snoc (SOME tele) (lbl, a) = interposeAfter (SOME tele) (#last tele, singleton (lbl, a))
    | snoc NONE (lbl, a) = singleton (lbl, a)

  fun map NONE f = NONE
    | map (SOME {first,last,preds,nexts,vals}) f =
        SOME
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
        Empty
      | Snoc of 'r * label * 'a

    fun out NONE = Empty
      | out (SOME {first,last,preds,nexts,vals}) =
          let
            val tail =
              case SOME (Dict.lookup preds last) handle _ => NONE of
                   NONE => NONE
                 | SOME pred =>
                     SOME
                       {first = first,
                        last = pred,
                        preds = preds,
                        nexts = nexts,
                        vals = vals}
          in
            Snoc (tail, last, Dict.lookup vals last)
          end

    fun into Empty = empty
      | into (Snoc (tel, lbl, a)) = snoc tel (lbl, a)
  end

  structure ConsView =
  struct
    type 'a telescope = 'a telescope
    type label = label

    datatype ('a, 'r) view =
        Empty
      | Cons of label * 'a * 'r

    fun out NONE = Empty
      | out (SOME {first,last,preds,nexts,vals}) =
          let
            val tail =
              case SOME (Dict.lookup nexts first) handle _ => NONE of
                   NONE => NONE
                 | SOME next =>
                     SOME
                      {first = next,
                       last = last,
                       preds = preds,
                       nexts = nexts,
                       vals = vals}
          in
            Cons (first, Dict.lookup vals first, tail)
          end

    fun outAfter NONE lbl = Empty
      | outAfter (SOME {first,last,preds,nexts,vals}) lbl =
         out (SOME
          {first = lbl,
           last = last,
           preds = preds,
           nexts = nexts,
           vals = vals})

    fun into Empty = empty
      | into (Cons (lbl, a, tele)) = cons (lbl,a) tele
  end

  local
    open ConsView
  in
    fun mapAfter NONE (lbl, f) = NONE
      | mapAfter (SOME tele) (lbl, f) =
          let
            val {first,last,preds,nexts,vals} = tele
            fun go Empty D = D
              | go (Cons (lbl, a, tele)) D =
                  go (out tele) (Dict.insert D lbl (f (Dict.lookup D lbl)))
          in
            SOME
              {first = first,
               last = last,
               preds = preds,
               nexts = nexts,
               vals = go (out (SOME tele)) vals}
          end

    fun remove tele lbl =
      let
        fun go Empty = into Empty
          | go (Cons (lbl', a, tele')) =
              if Label.eq (lbl, lbl') then
                go (out tele')
              else
                into (Cons (lbl', a, go (out tele')))
      in
        go (out tele)
      end



    fun toString pretty tele =
      let
        fun go Empty r = r
          | go (Cons (lbl, a, tele')) r =
              go (out tele') (r ^ ", " ^ L.toString lbl ^ " : " ^ pretty a)
      in
        go (out tele) "\194\183"
      end
  end

  local
    open SnocView
  in
    fun search (tele : 'a telescope) phi =
      let
        fun go Empty = NONE
          | go (Snoc (tele', lbl, a)) =
              if phi a then
                SOME (lbl, a)
              else
                go (out tele')
      in
        go (out tele)
      end

    fun subtelescope test (t1, t2) =
      let
        fun go Empty = true
          | go (Snoc (t1', lbl, a)) =
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

functor TelescopeNotation (T : TELESCOPE) : TELESCOPE_NOTATION =
struct
  open T

  fun >: (tele, p) = snoc tele p
end
