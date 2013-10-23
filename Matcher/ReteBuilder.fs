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
    open CoreLib.Trie

    let build trie =
        let amems = ref []
        let emitAmem cond join = amems := Util.updateMany !amems cond join

        let rec trie2toRete (trie:trie<Condition,string>) l =
            let getVars (Eq((var,_),_)) = [var]
            
            let trieEdgeToJoinNode (c,trie) =
                let tests = 
                    match l with
                        [] -> []
                        | _ -> [mkTest (Identifier,0,Identifier)]
                let join = mkJoin tests (trie2toRete trie ((getVars c)::l))
                (emitAmem c join;join)
            match trie with
                  Trie(prods,[]) -> List.map mkProd prods
                | Trie(prods,tries) -> (List.map mkProd prods) @ [mkBetaMem (List.map trieEdgeToJoinNode tries)]            
 
        let rete = match trie2toRete trie [] with
                    [{nodeType = Beta mem;children = children}] -> mkBetaMemDummy children
                    | children -> mkBetaMemDummy children
        (rete,List.map (fun (c,joins)->(c,mkAlphaMem joins))!amems)

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

        let _ = setParents rete
        let _ = List.iter (fun (_,a)->setAlphaMen a) alphas
        (rete,List.map (fun (Eq((objVar,var),value),mem) -> ((var,value),mem)) alphas)

    let buildReteFromProductions productions = buildSetParents <| buildTrie productions
