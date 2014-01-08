namespace Matcher

module SimpleBuilder =
    type MetaExp = Var of string | Val of string
    
    type Condition = MetaExp * MetaExp * MetaExp
   // type Rule = 
    let example : Condition list = [(Var "x", Val "on", Var "y");
                                    (Var "y", Val "left-of", Var "z");
                                    (Var "z", Val "color", Val "red");
                                    ]
    open ReteData
    open CoreLib

    let getField var (cond:Condition) =
        match cond with
              (Var v,_,_) when v = var -> Some Identifier
            | (_,Var v,_) when v = var -> Some Attribute
            | (_,_,Var v) when v = var -> Some Value
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

    let calcTests' exp field1 prevConds =
        match exp with
            Var var ->
                match getOffsetAndField var prevConds with
                    None -> []
                    | Some (index,field2) -> [mkTest(field1,index,field2)]
            | _ -> []

    let calcTests (id,attr,value) prevConds =
        List.concat [calcTests' id Identifier prevConds;calcTests' attr Attribute prevConds;calcTests' value Value prevConds]

    let rec insertSet v vs =
        match vs with
            [] -> [v]
            | v' :: vs' -> if v = v' then vs else v'::insertSet v vs'
    
    let rec updateMany l k v =
        match l with
            [] -> [(k,[v])]
            | (k',vs')::l' -> if k = k' then (k,insertSet v vs')::l' else (k',vs')::updateMany l' k v
            
    let build (conds: Condition list)  =
        let amems = ref []
        let emitAmem c join = amems := updateMany !amems c (mkAlphaMem join)
        
        let rec simpleToRete conds prevConds = 
            match conds with
                [] -> mkProd "test"
                | c :: cs ->
                    let child = simpleToRete cs (c::prevConds)
                    let tests = calcTests c prevConds
                    let join = mkJoin tests [child]
                    let _ = emitAmem c [join]
                    mkBetaMem [join]
        
        let {children = children} = simpleToRete conds []

        (mkBetaMemDummy children,!amems)

    let buildSetParents conds =
        let (rete, alphas) = build conds
        let _ = setParents rete
        let _ = List.iter (fun (_,a)->List.iter setAlphaMen a) alphas
        (rete,alphas)
