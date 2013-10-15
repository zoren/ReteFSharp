// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.


#r @"bin\\Debug\Matcher.dll"

//open MatcherTest
//open MatcherTest.ExampleProds
//open Matcher.ProdLang

#load "ExampleProds.fs"
//#load "ExampleProds.fs"

#load "ProdLangEvalTester.fs"
#load "ReteTester.fs"
//#load "ReteTester.fs"

// Define your library scripting code here
//let ass1 = assignEval "O"
//ass1 "X" 1
//ass1 "Y" 2
//ass1 "Z" 3
//ass1 "V" 4
//ass1 "U" 5
open MatcherTest.ProdLangEvalTester

////
////open MatcherTest
////open MatcherTest.ExampleProds
//open MatcherTest.ProdLangEvalTester
//
assign1()
assign2()
//assign3()
//assign4()
//assign5()

open MatcherTest.ReteTester

assignRete3()
assignRete2()
assignRete5()
assignRete4()
assignRete1()
