namespace ProdLang0

module SimpleBuilder =
    type MetaExp = Var of string | Val of string
    
    type exp = 
        StringConst of string
        | Variable of string * string
        | Concat of exp * exp
            
    type testExpression = Equals of exp * exp | LessThan of exp * exp    

    type Condition = MetaExp * MetaExp * MetaExp * testExpression list
    type SimpleProduction = Condition list * string

   // type Rule = 
    open Matcher.ReteData
    open CoreLib

    let getField var (cond:Condition) =
        match cond with
              (Var v,_,_,_) when v = var -> Some Identifier
            | (_,Var v,_,_) when v = var -> Some Attribute
            | (_,_,Var v,_ ) when v = var -> Some Value
            | _ -> None

    let rec getOffsetAndField var prevConds =
        match prevConds with
            [] -> None
            | cond :: conds ->
                match getField var cond with
                    Some field -> Some(0,field)
                    | None -> 
                        match getOffsetAndField var conds with
                            Some (n,f) -> Some(n+1,f)
                            | None -> None

    let mkSimpleEqTest (f1,index,f2) = 
        REquals(RVariable(Local,f1),RVariable(TokenOffset index,f2))

    let calcTests' exp field1 prevConds =
        match exp with
            Var var ->
                match getOffsetAndField var prevConds with
                    None -> []
                    | Some (index,field2) -> [mkSimpleEqTest(field1,index,field2)]
            | _ -> []

    let rec translateExp e =
        match e with
            StringConst s -> RStringConst s
//            | Variable (id,var) -> 
//                let 
//            RVariable()
            | Concat (e1,e2) -> RConcat(translateExp e1, translateExp e2)

    let rec translateTest test = 
        match test with
            Equals(e1,e2) -> REquals(translateExp e1,translateExp e2)
            |LessThan(e1,e2) -> RLessThan(translateExp e1,translateExp e2)

    let calcTests (id,attr,value,_) prevConds =
        List.concat [calcTests' id Identifier prevConds;calcTests' attr Attribute prevConds;calcTests' value Value prevConds]

    let rec insertSet v vs =
        match vs with
            [] -> [v]
            | v' :: vs' -> if v = v' then vs else v'::insertSet v vs'
    
    let rec updateMany l k v =
        match l with
            [] -> [(k,[v])]
            | (k',vs')::l' -> if k = k' then (k,insertSet v vs')::l' else (k',vs')::updateMany l' k v
            
    let condToOptionTripel (id,attr,value,_) = 
        let valToString mexp =
            match mexp with
                Val s -> Some s
                | _ -> None
        (valToString id, valToString attr, valToString value)

    let build (conds: Condition list)  =
        let amems = ref []
        let emitAmem c join = amems := updateMany !amems c (mkAlphaMem join)
        
        let rec simpleToRete (conds: Condition list) prevConds = 
            match conds with
                [] -> mkProd "test"
                | c :: cs ->
                    let child = simpleToRete cs (c::prevConds)
                    let tests = calcTests c prevConds
                    let join = mkJoin tests [child]
                    let _ = emitAmem (condToOptionTripel c) [join]
                    mkBetaMem [join]
        
        let {children = children} = simpleToRete conds []

        (mkBetaMemDummy children,!amems)

    open CoreLib.Trie
    let buildFromTrie (rules:trie<Condition,string>) =
        let amems = ref []
        let emitAmem c join = amems := updateMany !amems c join

        let rec trieToRete prevConds (Trie(prodNames, trieChildren):trie<Condition,string>) =
            let trieChildToRete (c:Condition,childTrie) =
                let tests = calcTests c prevConds
                let child = trieToRete (c::prevConds) childTrie

                let join = mkJoin tests [child]
                let _ = emitAmem c join
                join
            let prodNodes = List.map mkProd prodNames
            let mem = List.map trieChildToRete trieChildren
            mkBetaMem(prodNodes @ mem)

        let {children = children} = trieToRete [] rules

        (mkBetaMemDummy children, List.map (fun(c,j) ->(c,mkAlphaMem j)) !amems)

    let build2 prods = buildFromTrie <| buildTrie prods

    let build2SetParents prods =
        let (rete, alphas) = build2 prods
        let _ = setParents rete
        let _ = List.iter (fun (_,a)->setAlphaMen a) alphas
        (rete,alphas)

    let buildSetParents conds =
        let (rete, alphas) = build conds
        let _ = setParents rete
        let _ = List.iter (fun (_,a)->List.iter setAlphaMen a) alphas
        (rete,alphas)

    let metaExpMatch metaExp v =
        match metaExp with
            Val value -> value = v
            | Var _ -> true

    let wmeMatch { fields = (wid,wattr,wvalue) } ((id,attr,value):Condition) =
        metaExpMatch id wid && metaExpMatch attr wattr && metaExpMatch value wvalue

    let lookupAlphaMem l w = List.tryFind ( fun(c,v) -> wmeMatch w c ) l


    //
//    open Matcher.SimpleBuilder    
//
//    let act alphas wme =         
//        match lookupAlphaMem alphas wme with
//            Some (_,alphaMems) -> List.iter (actMem wme) alphaMems
//            | None -> failwith "could not find matching condition"
//
//    let act2 alphas wme =
//        match lookupAlphaMem alphas wme with
//            Some (_,alphaMems) -> actMem wme alphaMems
//            | None -> failwith "could not find matching condition"
