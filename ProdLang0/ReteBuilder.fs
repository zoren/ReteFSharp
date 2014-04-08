namespace ProdLang0

module ReteBuilder =
    open Matcher.ReteData

    open ProdLang0.ProdLang
    open CoreLib

    type VarIndex = int
    type ReteConditionExpression = ReteVariable of VarIndex | ReteValue of Value
    type ReteCondition = VarIndex * Variable * ReteConditionExpression

    let variablesToNumbers conds =
        let varMap = ref []
        let lookupUpdateMap obj =
          match Util.lookupOpt !varMap obj with
            Some index -> index
            | None ->
                let index = match !varMap with [] -> 0 | ((_,i)::_) -> i + 1
                varMap := (obj,index)::!varMap
                index
        let convertCondition cond =
          match cond with
            Eq((obj,var),ExpVariable metaVar) -> (lookupUpdateMap obj,var,ReteVariable(lookupUpdateMap metaVar))
            | Eq((obj,var),ExpValue value) -> (lookupUpdateMap obj,var,ReteValue value)
            | _ -> raise (new System.ArgumentOutOfRangeException(""))
        List.map convertCondition conds

    open CoreLib.Trie

    let tryFindValueIndex f l =
      match Seq.tryFindIndex f l with
        Some i -> Some(Seq.find f l,i)
        | None -> None

    let build trie =
        let amems = ref []
        let mapVarToAnything exp = match exp with ReteVariable _ -> WMEAnyThing | ReteValue v -> WMEValue v
        let emitAmem (_,var,value) join = amems := Util.updateMany !amems (var,mapVarToAnything value) join

        let getVars ((objVar,_,exp):ReteCondition) =
          (objVar,Identifier) :: match exp with
                                  ReteVariable var -> [(var,fieldOfArg.Value)]
                                  | _ -> []

        let rec trie2toRete (trie:trie<ReteCondition,ProductionId>) (l:(VarIndex * fieldOfArg) list list) =
            let rec tryFindVarIndex varMap varIndex =
              match varMap with
                [] -> None
                | (var,field) :: varMap' when varIndex = var -> Some(field, 0)
                | _ :: varMap' -> tryFindVarIndex varMap' varIndex

            let trieEdgeToJoinNode (c : ReteCondition,trie) =
                let createTestsForField (varIndex, localField) =
                  match tryFindValueIndex (List.exists (fun(var,_) -> var = varIndex)) l with
                          Some (condVars,index) ->
                            let (_,f) = List.find (fun (key,_)-> varIndex = key) condVars
                            Some (mkTest (localField,index, f))
                        | None -> None
                let conditionVars = getVars c
                let tests = Util.mapPartial createTestsForField conditionVars
                let join = mkJoin tests (trie2toRete trie (conditionVars::l))
                (emitAmem c join;join)
            match trie with
                  Trie(prods,[]) -> List.map mkProd prods
                | Trie(prods,tries) -> (List.map mkProd prods) @ [mkBetaMem (List.map trieEdgeToJoinNode tries)]

        let rete = match trie2toRete trie [] with
                    [{nodeType = Beta mem;children = children}] -> mkBetaMemDummy children
                    | children -> mkBetaMemDummy children
        (rete,List.map (fun (c,joins)->(c,mkAlphaMem joins)) !amems)

    let buildFromTrie trie =
        let (rete,alphas) = build trie

        let getVarValMemTuple = function | (Eq((_,var),value),mem) -> Some ((var,value),mem) | (TRUE,_) -> None
        (rete,List.map (fun ((var,value),mem) -> ((var,value),mem)) alphas)

    let buildReteFromProductions productions =
        let filteredProds = Seq.map (fun((conds,prodName):Production) -> (variablesToNumbers conds, prodName)) productions
        buildFromTrie <| buildTrie filteredProds

    let buildReteFromSystem {productions = productions } = buildReteFromProductions productions
