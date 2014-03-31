namespace MatcherTest

open MatcherTest.ExampleProds


module ReteTester =
    let (reteDummy, alphas) = ProdLang0.ReteBuilder.buildReteFromProductions testProds

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