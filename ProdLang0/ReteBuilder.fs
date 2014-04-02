namespace ProdLang0

module ReteBuilder =
    open Matcher.ReteData
    
    open ProdLang0.ProdLang
    open CoreLib
    
    type VarIndex = int
    type ReteCondition = VarIndex * Variable * Value

    let conditionsToNumbers conds =
        let rec helper objectVars cs = 
            match cs with 
                [] -> []
                | (Eq((obj,var),value):: conds) ->
                match Util.lookupOpt objectVars obj with
                        Some index -> 
                            ((index,var,value): ReteCondition) :: helper objectVars conds
                        | None -> 
                            let index = match objectVars with
                                            [] -> 0
                                            | ((_,i)::_) -> i + 1
                            (index,var,value) :: helper ((obj,index)::objectVars) conds

        helper [] (List.filter ((<>)TRUE) conds)
    
    open CoreLib.Trie
    
    let build trie =
        let amems = ref []
        let emitAmem (_,var,value) join = amems := Util.updateMany !amems (var,value) join

        let rec trie2toRete (trie:trie<ReteCondition,string>) l =
            let getVars = function (var,_,_) -> [var]
            let trieEdgeToJoinNode ((varIndex,_,_) as c,trie) =
                let tests = 
                    match List.tryFindIndex (List.exists ((=) varIndex)) l with
                        Some index -> [mkTest (Identifier,index,Identifier)]
                        | None -> []
                let join = mkJoin tests (trie2toRete trie ((getVars c)::l))
                (emitAmem c join;join)
            match trie with
                  Trie(prods,[]) -> List.map mkProd prods
                | Trie(prods,tries) -> (List.map mkProd prods) @ [mkBetaMem (List.map trieEdgeToJoinNode tries)]            

        let rete = match trie2toRete trie [] with
                    [{nodeType = Beta mem;children = children}] -> mkBetaMemDummy children
                    | children -> mkBetaMemDummy children
        (rete,List.map (fun (c,joins)->(c,mkAlphaMem joins)) !amems)

    let buildSetParents trie = 
        let (rete,alphas) = build trie
        
        let getVarValMemTuple = function | (Eq((_,var),value),mem) -> Some ((var,value),mem) | (TRUE,_) -> None
        let _ = setParents rete
        let _ = List.iter (fun (_,a)->setAlphaMem a) alphas
        (rete,List.map (fun ((var,value),mem) -> ((var,value),mem)) alphas)

    let buildReteFromProductions productions = 
        let filteredProds = Seq.map (fun((conds,prodName):Production) -> (conditionsToNumbers conds, prodName)) productions
        buildSetParents <| buildTrie filteredProds

    let buildReteFromSystem {productions = productions } = buildReteFromProductions productions
