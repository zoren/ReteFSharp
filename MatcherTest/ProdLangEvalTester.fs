namespace MatcherTest

open MatcherTest.ExampleProds

module ProdLangEvalTester = 
//    open Matcher.ProdLangEval
    open Matcher.MultiInstEvaluator

    let mySys = mkSystem testProds
    let myEnv = mkState mySys
    let myInstance = getNextInstance myEnv
    let myInstanceState = (myEnv,myInstance)
        
    let assign1 () = assignState myInstanceState "X" "1"
    let assign2 () = assignState myInstanceState "Y" "2"
    let assign3 () = assignState myInstanceState "Z" "3"
    let assign4 () = assignState myInstanceState "V" "4"
    let assign5 () = assignState myInstanceState "U" "5"

//    let assignEval objectId var value = assignState myState (objectId,var) (value.ToString())
