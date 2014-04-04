namespace MatcherTest

open MatcherTest.ExampleProds


module ReteTester =
    open Matcher.ReteData
    open Matcher.Runner

    let activateCond alphas inst variable value =
        let tup = (inst,variable,value)
        let wme = { fields = tup }
        match CoreLib.Util.lookupOpt alphas (variable,value) with
            Some alphaMem ->
                // if wme already assigned do nothing
                if List.exists ((=)wme) !alphaMem.items then
                    ()
                else
                    alphaMemoryActivation(alphaMem, wme)
            | None -> ()

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

    let (reteDummy, alphas) = ProdLang0.ReteBuilder.buildReteFromProductions testProds
    let _ = Matcher.ReteData.setBackPointers (reteDummy, alphas)

    open Matcher.Runner

    let assignRete = activateCond alphas

    let assignReteInt (inst:int)= assignRete ("$" + inst.ToString())
    let assignReteInst0 = assignReteInt 1
    let assignRete1() = assignReteInst0 "X" "1"
    let assignRete2() = assignReteInst0 "Y" "2"
    let assignRete3() = assignReteInst0 "Z" "3"
    let assignRete4() = assignReteInst0 "V" "4"
    let assignRete5() = assignReteInst0 "U" "5"

    let dumpStateRete () = getProductionNodes reteDummy