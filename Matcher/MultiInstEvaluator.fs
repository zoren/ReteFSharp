namespace Matcher

module MultiInstEvaluator =
    open Matcher.ProdLang

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
        (Util.lookupOpt env (inst, var)) = Some value

    let evalConds conds env =
        let insts = Seq.distinct <| Seq.map (fun ((instId,_),_) -> instId) env
        let objMaps = getObjMaps conds insts
        let evalCondMap objMap = Seq.fold (&&) true (Seq.map (fun c -> evalCond objMap env c) conds)
        Seq.filter (fun objMap -> (evalCondMap objMap) ) objMaps

    let tryGetEvalEnvCond objMap (env:Environment) (Eq((objVar,var),value)) =
        match Util.lookupOpt objMap objVar with
            Some inst ->
                if Seq.exists ((=) ((inst, var),value)) env then
                     Some (inst,var, value)
                else None
            | None -> None

    let tryGetEvalEnvConds conds env =
        let insts = Seq.distinct <| Seq.map (fun ((instId,_),_) -> instId) env
        let objMaps = getObjMaps conds insts
        let rec tryGetEvalObjMapEnvConds conds' objMap =
            match conds' with
                [] -> Some []
                | c::cs ->
                    match tryGetEvalEnvCond objMap env c with
                        Some s -> Option.bind (fun l -> Some (s::l)) (tryGetEvalObjMapEnvConds cs objMap)
                        | None -> None
        Seq.choose (tryGetEvalObjMapEnvConds conds) objMaps

    let evalState { env = env; system = {productions = productions}} =
        (Seq.map (fun (conds,prodName) -> (prodName,evalConds conds !env)) productions,
         Seq.map (fun (conds,prodName) -> (prodName,tryGetEvalEnvConds conds !env)) productions)
    let evalStateList { env = env; system = {productions = productions}} : (string * (int * string * string) list list) list =
        List.map (fun (conds,prodName) -> (prodName,Seq.toList <| tryGetEvalEnvConds conds !env)) productions
    let mkSystem prods = {productions = prods}
    
    let mkState sys = {env = ref []; system = sys}

    let getNextInstance {env = envRef } =
        let env = !envRef
        match env with
            [] -> 1
            | _ -> Seq.max <| Seq.map (fun((instId,_),_)-> instId) env

    let assign (s : State) instId var value = s.env := Util.insertSet ((instId, var), value) (!s.env)

    let assignState (state:State,instId) var value = assign state instId var value

    let resetInstance state instId = state.env := List.filter (fun((inst,_),_) -> inst <> instId) (!state.env)