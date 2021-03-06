﻿namespace ProdLang0

module MultiInstEvaluator =
    open ProdLang
    open CoreLib
    type InstanceId = int
    type Environment = ((InstanceId * Variable) * Value) list

    type State = {
        env : Environment ref
        system : System
        }

    let getObjVar = function | Eq((objVar,_),_) -> Some objVar | _ -> None

    let getObjVars conds = Seq.toList <| (Seq.distinct <| Seq.choose getObjVar conds)

    let getObjMaps conds insts =
        let objVars = getObjVars conds
        let combs = Util.generateCombinations (Seq.length objVars) insts
        Seq.map (fun l -> List.zip objVars l) combs

    let tryGetEvalEnvCond objMap (env:Environment) cond =
        match cond with
            (Eq((objVar,var),value)) ->
                match Util.lookupOpt objMap objVar with
                    Some inst ->
                        match Util.lookupOpt env (inst,var) with
                          Some value -> Some (inst,var, value)
                          | None -> None
                    | None -> None
            | TRUE -> None

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

    type ProductionState = (InstanceId * Variable * Value) list
    type ProductionsState = (ProductionId * ProductionState list) list
    let evalStateList { env = env; system = {productions = productions}} : ProductionsState =
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
