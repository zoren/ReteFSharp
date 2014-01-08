namespace Matcher


module ReteBuilder =
    open ReteData
    
    let mkNullParent () = ref None

    let mkRete nodeType children = {nodeType = nodeType;children = children;parent = mkNullParent ()}
     
    let mkProd s = mkRete (Production (s, ref [])) []
    
    let mkTest (farg1,cond,farg2) = { fieldOfArg1 = farg1;conditionNumberOfArg2 = cond; fieldOfArg2 = farg2 }
    let mkNullAlpha () = ref None
    let mkJoin tests children = mkRete (Join {tests = tests;amem = mkNullAlpha ()}) children
    
    let mkBetaMem children = mkRete (Beta {items = ref []}) children
    let mkBetaMemDummy children = mkRete (Beta {items = ref [[]]}) children

    let mkAlphaMem children = {items = ref [];successors = children}   

    open ProdLang
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
        
        let rec setParents (node:reteNode) =
            for child in node.children do
                (child.parent := Some node;setParents child)

        let setAlphaMen (node:alphaMemory) =
            for succ in node.successors do
                match succ.nodeType with
                    Join jd -> jd.amem := Some node
                    | _ -> ()
        let getVarValMemTuple = function | (Eq((_,var),value),mem) -> Some ((var,value),mem) | (TRUE,_) -> None
        let _ = setParents rete
        let _ = List.iter (fun (_,a)->setAlphaMen a) alphas
        (rete,List.map (fun ((var,value),mem) -> ((var,value),mem)) alphas)

    let buildReteFromProductions productions = 
        let filteredProds = Seq.map (fun(conds,prodName) -> (conditionsToNumbers conds, prodName)) productions
        buildSetParents <| buildTrie filteredProds
