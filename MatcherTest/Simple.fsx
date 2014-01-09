#r @"bin\\Debug\Matcher.dll"


open Matcher.Runner

open Matcher.SimpleBuilder
#load "SimpleExample.fs"
open MatcherTest.SimpleExample

let (dummy,alphas) = buildSetParents example   

let _ = act alphas w1
let _ = act alphas w5
let _ = act alphas w9

