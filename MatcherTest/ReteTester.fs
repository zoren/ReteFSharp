namespace MatcherTest

module ReteTester =
    open Matcher.ReteData
    open Matcher

    let matchWMEPattern wmeSymbol pattern =
      match pattern with
        WMEValue v when v = wmeSymbol -> true
        | WMEAnyThing -> true
        | _ -> false

    let lookupAlphaMem (alphas:((symbol * symbol) * alphaMemory) list) variable value =
      CoreLib.Util.lookupOpt alphas (variable,value)

    let matchAlphaMems alphas variable value = List.filter (fun ((var, pat),_) -> variable = var && matchWMEPattern value pat) alphas

    let activateCond alphas inst variable value =
        let matchingAlphas = matchAlphaMems alphas variable value
        let tup = (inst,variable,value)
        let wme = { fields = tup }
        let runner = new Runner()
        let activate alphaMem = if List.exists ((=)wme) !alphaMem.items then
                                        ()
                                   else
                                        runner.activateAlphaMemory(alphaMem, wme)
        List.iter activate <| List.map (fun (_,alpha) -> alpha) matchingAlphas

    let rec getProductionNodes ({nodeType = nodeType;children = children} as node) : (string * (int * string * string) list list) list =
        let deWME ({fields = (instString,var,value)}) =
            let inst = System.Int32.Parse(instString.Substring(1))
            (inst,var,value)
        let deToken token = List.map deWME token
        let deMatch (token, wme) = (List.rev (deToken token)) @ [deWME wme]
        match nodeType with
            Production (prodName,matches) ->
                (prodName, List.map deMatch (!matches)) :: List.collect getProductionNodes children
            | _ -> List.collect getProductionNodes children
