namespace Matcher

module ProdLangEval =
    open ProdLang
    type Environment = (ObjectVar * Value) list

    type State = {
        env : Environment ref
        system : System
        }

    let rec update l k v =
        match l with
            [] -> [(k,v)]
            | (k',v)::l' -> if k = k' then (k',v)::l' else (k',v)::update l' k v
    
    let rec updateMultiValue l k v =  Seq.toList <| Seq.distinct ((k,v)::l)
    
    let evalCondition (env:Environment) cond =
        match cond with
            Eq(ovar,value) -> 
                Seq.exists (fun x -> x = (ovar,value)) env
                    

    let rec evalConditions env conds = Seq.fold (&&) true (Seq.map (fun c -> evalCondition env c) conds)
    let mkSystem prods = {productions = prods}
    let mkState sys = {env = ref [];system = sys}

  
    let assign (s : State) (ovar:ObjectVar) value = s.env := updateMultiValue (!s.env) ovar value
    let print (s:string) = System.Console.WriteLine(s)
    
    let getActiveProductions env prods = Seq.filter (fun (conds,p) -> evalConditions env conds) prods
    
    let dumpProductions ps = Seq.iter (fun (_,p)-> print ("Production activated: " +  p)) ps

    let dumpActiveProds env prods = dumpProductions <| getActiveProductions env prods

    let getActiveProductionsState (state:State) = Seq.toList <| getActiveProductions (!state.env) (state.system.productions)
    let dumpActive state = dumpProductions <| getActiveProductionsState state
    let assignState (state:State) (ovar:ObjectVar) value = assign state ovar value;dumpActive state

    let resetState (state:State) = state.env := []

    let X = ("O","X")
    let C1 = Eq(X,"1")
    let Y = ("O","Y")
    let C2 = Eq(Y,"2")
    let Z = ("O","Z")
    let C3 = Eq(Z,"3")
    let V = ("O","V")
    let C4 = Eq(V,"4")
    let U = ("O","U")
    let C5 = Eq(U,"5")

    let prod1 = ([C1;C2;C3],"P1")
    let prod2 = ([C1;C2;C4;C5],"P2")
    let prod3 = ([C1;C2;C4;C3],"P3")

    let prods = [prod1;prod2;prod3]

    let mySys = mkSystem prods
    let myState = mkState mySys

    let _ = assignState myState X "1"
    let _ = assignState myState Y "2"
    let _ = assignState myState Z "3"


    