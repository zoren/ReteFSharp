namespace MatcherTest

module CompareWithReference =
    open Matcher.ProdLang
   
    open CoreLib

    let getComparisonFromCond cond = 
        match cond with
            Eq((obj,var),value) -> Seq.singleton (var,value)
            | TRUE -> Seq.empty

    let getObjVarsFromConds trie conds = 
        let comparisons = Seq.collect getComparisonFromCond conds
        Seq.fold (fun s (var,value) -> Trie.insert s (Seq.toList var, value) ) trie comparisons
        
    let getVarsFromProds (prods: Production list) = Seq.fold (fun s (conds,_) -> getObjVarsFromConds s conds) Trie.empty prods

    let getVarValueAssocList prods = 
        List.map (fun(kl:char list,vl)->(new string(List.toArray kl),vl))(Trie.toAssocList <| getVarsFromProds prods)

    let pickRandomFromList list =
        let length = List.length list
        let rnd = System.Random()
        let index = rnd.Next(length)
        List.nth list index

    let pickRandomFromRange lower upper =
        let rnd = System.Random()
        rnd.Next(lower,upper)        

    let flattenAssocList assocList = List.collect (fun(k,vs)-> List.map (fun v -> (k,v)) vs) assocList 

    let getObjVarsFromCond cond = 
        match cond with
            Eq((obj,var),value) -> Seq.singleton obj
            | TRUE -> Seq.empty

    let getObjVarsFromProductions (prods:seq<Production>) = Seq.collect (fun (conds,_) -> Seq.collect getObjVarsFromCond conds) prods

    open Matcher.Runner
    open Matcher.MultiInstEvaluator
    
    let referenceAndReteAgree { productions = prods } = 
        let numberOfProds = List.length prods
        let domain = flattenAssocList <| getVarValueAssocList prods
        
        // setup rete
        let (reteDummy, alphas) = Matcher.ReteBuilder.buildReteFromProductions prods
        let assignReteInt (inst:int) = activateCond alphas ("$" + inst.ToString())
        
        // setup classic
        let mySys = mkSystem prods
        let myEnv = mkState mySys
        let myInstance = getNextInstance myEnv

        let assignReference inst var value = assignState (myEnv,inst) var value
        
        let normalizeState state = List.map (fun (prodName,states) -> (prodName, List.sort states)) state

        let assign inst var value = (assignReteInt inst var value;assignReference inst var value)

        let objvars = getObjVarsFromProductions prods

        let assignRandom () = 
            let inst = System.Random().Next(1,Seq.length objvars)
            let (var,value) = pickRandomFromList domain
            assign inst var value

        let rec activateAllProds() =
            let referenceState = normalizeState <| evalStateList myEnv
            let reteState = normalizeState <| getProductionNodes reteDummy            
            if referenceState = reteState then
                if List.length referenceState = numberOfProds then
                    ()
                else
                    (assignRandom();activateAllProds())
            else
                failwith ""
        activateAllProds()
  

