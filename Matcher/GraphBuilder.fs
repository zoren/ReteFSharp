namespace Matcher

module GraphBuilder =
    open ProdLang

    type PrefixTree<'a> = PTree of 'a * (PrefixTree<'a> seq)

    let filterEmpty l = Seq.filter (fun k -> not(List.isEmpty k)) l
    
    let listGroupHead l = Seq.toList <| Seq.groupBy (fun x -> Seq.head x) (filterEmpty l)
    
    let rec removeHeads l = Seq.map (fun(x,post)->PTree(x,removeHeads(filterEmpty <| Seq.map (List.tail) post))) (listGroupHead l)
    
    type PrefixTreeData<'a,'b> = PTreeData of 'a * (PrefixTreeData<'a,'b> list) | Data of 'b

    let makePrefixTreeData (assoc:(int list * string)list) =
//        let filterEmpty = Seq.m
        let groups = Seq.groupBy (fun (l,d)-> Seq.head l) assoc
        let tails = Seq.toList <| Seq.map (fun (i,d)-> (i,Seq.toList <| Seq.map (fun (il,s)-> (List.tail il,s)) d)) groups
        let func i d = 
            match d with
                [] -> Data d
        //        | _ -> PTreeData(i,Seq.map makePrefixTreeData i)
        tails
//
//    let rec mkPTD (il,s) =
//        match il with
//            [] -> Data s
//            | i::il' -> 
//    and mkPTDs (assoc:(int list * string)list) =3

    let fe list = 
        let (empty, nonEmpty) = List.partition (fun(l,s)-> List.isEmpty l) list
        (List.map (fun (_,s)-> Data s)empty,nonEmpty)
    
    let lgh l = 
        let (es,f) = fe l
        (es,Seq.toList <| Seq.groupBy (fun (x,_) -> Seq.head x) f)

    let rec rh l = 
        let (completed,y) = lgh l
        let yy = List.map (fun (x,y) -> (x,Seq.toList y)) y
        let ret = List.map (fun(x,post)-> PTreeData(x,rh (List.map (fun (x,y)->(List.tail x,y)) post))) yy
        completed @ ret
        //Seq.map (fun(x,post)->PTreeData(x,rh(fe (List.map (fun (x,_) -> List.tail x) post)))) y
    
    type group<'k,'v> = Empty of 'v | Group of 'k * ('k list * 'v) list
    
    type trie<'a,'b> = Trie of 'a * (trie<'a,'b> list) | TrieLeaf of 'b list
    type trie2<'a,'b> = Trie2 of 'b list * (('a * trie2<'a,'b>)list)

    let optHead l = 
        match l with
            [] -> None
            | (h::_) -> Some h
    
    let groupBy f (l:_ list) = Seq.toList ((Seq.map (fun (a,b) -> (a,Seq.toList b))) <| Seq.groupBy f l)

    let groupByFirst l = groupBy (fun (k,v) -> optHead k) l

//    let rec makeTrie2 (assocList:('k list * 'v) list) = 
//        let mkt2' l (empties,vals) =
//            match l with
//                ([],v)::l' -> (v::empties,vals)
//                | (ks,v)::l' -> (empties,)
//                | _ -> (empties,vals)

    let rec lookupTrie2 (Trie2(values,tries)) kl =
        match kl with
            [] -> Some values
            | (k'::kl') -> 
                match Util.lookupOpt tries k' with
                    None -> None
                    | Some trie -> lookupTrie2 trie kl'

    let rec insert (Trie2(values,tries)) (kl, v) =
        match kl with
            [] -> Trie2(Util.insertSet v values,tries)
            | (k'::kl') -> 
                let rec insertTries subTries =
                    match subTries with
                        [] -> [(k',insert (Trie2([],[])) (kl', v) )]
                        | (subK,trie) :: sts -> if subK = k' then (subK,insert trie (kl', v))::sts else (subK,trie) :: insertTries sts
                Trie2(values, insertTries tries)

    let buildTrie assocList = List.fold insert (Trie2([],[])) assocList

//
//
//    let rec nonIncr assocList = 
//        match assocList with
//            [] -> Trie2 ([], [])
//            | (kvp::l) ->
//                let Trie2(empties,(Trie2(kl',vs') as trie')::tries') = nonIncr l
//                match kvp with
//                    ([],v) -> Trie2(v::empties,tries)
//                    | (k::kl,v) -> 
//                        let tries = if Some k = optHead kl' then Trie2()::tries else Trie2()::trie'::tries
//                        Trie2(empties,tries)
//
//    let rec makeTrie (assocList:('k list * 'v) list) = 
//        let groups = groupByFirst assocList
//        let makeTrieForGroup group =
//            match group with
//                (None, al) -> TrieLeaf (List.map (fun (_,v) -> v) al)
//                | (Some k, al) ->
//                    let l = List.map (fun (k,v) -> (List.tail k,v)) al
//                    Trie(k,makeTrie l)
//        List.map makeTrieForGroup groups
//    
//    open ReteData
//    
//    let mkRete nodeType children = {nodeType = nodeType;children = children;parent = ref nullRete}
//     
//    let mkProd s = mkRete (Production s) []
//    
//    let mkTests (cond:Condition) = []: testAtJoinNode list
//    let mkJoin tests children = mkRete (Join {tests = tests;amem = ref nullAlpha}) children
//    
//    let mkBetaMem children = mkRete (Beta {items = ref []}) children
//    let mkBetaMemDummy children = mkRete (Beta {items = ref [[]]}) children
//
//    let mkAlphaMem children = {items = ref [];successors = children}    
//    let build trie =
//        let amems = ref []
//        let emitAmem cond join = amems := Util.updateMany !amems cond join
//
//        let rec trie2toRete (trie:trie2<Condition,string>) =
//            let trieEdgeToJoinNode (c,trie) =
//                let join = mkJoin (mkTests c) (trie2toRete trie)
//                (emitAmem c join;join)
//            match trie with
//                  Trie2(prods,[]) -> List.map mkProd prods
//                | Trie2(prods,tries) -> (List.map mkProd prods) @ [mkBetaMem (List.map trieEdgeToJoinNode tries)]            
// 
//        let rete = match trie2toRete trie with
//                [{nodeType = Beta mem;children = children}] -> mkBetaMemDummy children
//                | children -> mkBetaMemDummy children
//        (rete,!amems)
//
//    let buildRete (conditionTrie:trie<Condition,string> list) =
//        let amems = ref []
//        let emitAmem cond amem = amems := Util.updateMany !amems cond amem 
//        let rec makeRete conditionTrie =        
//            match conditionTrie with
//                TrieLeaf prods -> List.map mkProd prods // todo wrong!! we should make only one rete node not a list
//                | Trie (cond,tries) -> 
//                    let subNodes = List.map makeRete tries
//                    let mkJoin' nodes = 
//                        let join = mkJoin (mkTests ()) nodes
//                        (emitAmem cond join;join)
//                    let joinNodes = List.map mkJoin' subNodes
//                    [mkBetaMem joinNodes]
//        (List.map makeRete conditionTrie, !amems)
//    
//    let buildReteFromProdRules prods = 
//        let (topNodes,alphas) = buildRete (makeTrie prods)
//        (mkBetaMemDummy <| Util.single topNodes, alphas)
//
//    let C = ("O","C")
//    let testProds : (Condition list*string) list= 
//        [([Eq(C,"1");Eq(C,"2");Eq(C,"3")],"P1");
//        ([Eq(C,"1");Eq(C,"2");Eq(C,"4");Eq(C,"5")],"P2");
//        ([Eq(C,"1");Eq(C,"2");Eq(C,"4");Eq(C,"3")],"P3")]
//
//    let testProdsSub : (Condition list*string) list= 
//        [([Eq(C,"3")],"P1");
//        ([Eq(C,"4");Eq(C,"5")],"P2");
//        ([Eq(C,"4");Eq(C,"3")],"P3")]
//
//
//    let groupByHead assocList =
//        match ass with
//            ([],value)::al' -> Empty value :: groupByHead al'
//            | (k::kl,value)::(k'::kl',value)::al' when k = k' -> [(k,)]
//    
//    let insertTrie trie (k,v) = 
//        match k with
//            [] -> 
//    let takeWhile 
//    let assocListToTrie ass = 
//        match ass with
//            ([],value)::al' -> Data value
//            | (k::kl,value)::al' -> PrefixTreeData(k, assocListToTrie)
//
//    let treed = rh [([1;2;3],"P1");([1;2;4;5],"P2");([1;2;4;3],"P3")]
//
//    let printTrie trie =
//        let rec printTrie n tree =
//            match tree with
//                Trie(v,subtrees) -> System.Console.WriteLine((spaces n) + v.ToString());Seq.iter (fun t -> printTrie (n+1) t) subtrees
//                | TrieLeaf p -> System.Console.WriteLine((spaces n) + p.ToString())
//        printTrie 0 trie
//    let printTries tries = List.iter printTrie tries
//
//    let printCond (Eq((o,var),value)) = o + "." + var + " = " + value
//    
//    let printTrie trie =
//        let wl (s:string) = System.Console.WriteLine s
//        let rec printTrie' n tree =
//            match tree with
//                Trie2(v,subtrees) -> wl((spaces n) + v.ToString());Seq.iter (fun (cond,t) -> wl((spaces n) + printCond cond);printTrie' (n+2) t) subtrees
//        printTrie' 0 trie
//
//    let rec buildRete (sortedProds: Production seq) = Seq.map (fun (k,v) -> PTree(k,buildRete sortedProds)) <| Seq.groupBy (fun x -> x) sortedProds
//
//    let rec spaces n = 
//        if n > 0 
//        then " " + (spaces (n-1))
//        else ""
//
//    let rec printPTD n tree =
//        match tree with
//            PTreeData(v,subtrees) -> System.Console.WriteLine((spaces n) + v.ToString());Seq.iter (fun t -> printPTD (n+1) t) subtrees
//            | Data p -> System.Console.WriteLine((spaces n) + p.ToString())
//
//    let rec printPT n (PTree(v,subtrees)) =
//        System.Console.WriteLine((spaces n) + v.ToString());Seq.iter (fun t -> printPT (n+1) t) subtrees
//
//    let printPTs ptrees = Seq.iter (printPT 0) ptrees
//
