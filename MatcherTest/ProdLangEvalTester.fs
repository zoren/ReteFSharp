namespace MatcherTest

open MatcherTest.ExampleProds

module ProdLangEvalTester = 
//    open Matcher.ProdLangEval
    open Matcher.MultiInstEvaluator

    let mySys = mkSystem testProds
    let myEnv = mkState mySys
    let myInstance = getNextInstance myEnv
    //let myInstanceState = (myEnv,myInstance)      
    let assignReference inst var value = assignState (myEnv,inst) var value
    //let assignEvalInst0 = assignState myInstanceState



//    let assign1 () = assignEvalInst0 "X" "1"
//    let assign2 () = assignEvalInst0 "Y" "2"
//    let assign3 () = assignEvalInst0 "Z" "3"
//    let assign4 () = assignEvalInst0 "V" "4"
//    let assign5 () = assignEvalInst0 "U" "5"

    let dumpState() = evalStateList myEnv

//    let assignEval objectId var value = assignState myState (objectId,var) (value.ToString())
