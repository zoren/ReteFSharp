namespace Matcher

module MultiInstEvaluator =
    open ProdLang

    type InstanceId = int
    type Environment = ((InstanceId * Variable) * Value) list

    type State = {
        env : Environment ref
        system : System
        }

    let getObjVars conds = Seq.toList <| (Seq.distinct <| Seq.map (fun(Eq((objVar,_),_)) -> objVar) conds)

    let getObjMaps conds insts =
        let objVars = getObjVars conds
        let combs = Util.generateCombinations (Seq.length objVars) insts
        Seq.map (fun l -> List.zip objVars l) combs
        
    let evalCond objMap (env:Environment) (Eq((objVar,var),value)) =
        let inst = Option.get <| Util.lookupOpt objMap objVar
        let lookupValue = Option.get <| Util.lookupOpt env (inst, var)
        lookupValue = value

    let evalConds conds env =
        let insts = List.map (fun ((instId,_),_) -> instId) env
        let objMaps = getObjMaps conds insts
        let evalCondMap objMap = Seq.fold (&&) true (Seq.map (fun c -> evalCond objMap env c) conds)
        Seq.filter (fun objMap -> (evalCondMap objMap) ) objMaps

    let evalState { env = env; system = {productions = productions}} =
        Seq.map (fun (conds,prodName) -> (prodName,evalConds conds !env)) productions

    let mkSystem prods = {productions = prods}
    
    let mkState sys = {env = ref []; system = sys}

    let getNextInstance {env = envRef } =
        let env = !envRef
        match env with
            [] -> 0
            | _ -> Seq.max <| Seq.map (fun((instId,_),_)-> instId) env

    let assign (s : State) instId var value = s.env := Util.insertSet ((instId, var), value) (!s.env)

    let assignState (state:State,instId) (var) value = assign state instId var value;evalState state


