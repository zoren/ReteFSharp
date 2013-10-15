﻿namespace MatcherTest

open MatcherTest.ExampleProds


module ReteTester =
    open Matcher.ReteBuilder

    let (reteDummy, alphas) = Matcher.ReteBuilder.buildReteFromProductions testProds

    open Matcher.Runner

    let assignRete = activateCond alphas
//    let resetRete () = List.iter (fun (_,alpha) -> resetAlpha alpha) alphas
    let inst = "$1"
    let assignReteInst0 = assignRete inst
    let assignRete1() = assignReteInst0 "X" "1"
    let assignRete2() = assignReteInst0 "Y" "2"
    let assignRete3() = assignReteInst0 "Z" "3"
    let assignRete4() = assignReteInst0 "V" "4"
    let assignRete5() = assignReteInst0 "U" "5"