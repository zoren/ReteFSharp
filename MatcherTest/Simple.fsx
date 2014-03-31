#r @"bin\\Debug\Matcher.dll"

open Matcher.Runner

open Matcher.SimpleBuilder

#load "SimpleExample.fs"

open MatcherTest.SimpleExample

let (dummy,alphas) = build2SetParents simpleExampleProds

let _ = act2 alphas w1
let _ = act2 alphas w5

let _ = act2 alphas w9
let _ = act2 alphas w10


