#r @"bin\Debug\CoreLib.dll"

#load "ReteData.fs"
#load "Dumper.fs"

open Matcher.ReteData
open Matcher.Dumper

let productionNode = (mkProd "P1")

let graph = Matcher.Dumper.buildGraphviz (productionNode, [])

let joinNode = mkJoin [] [productionNode]

let joinGraph = Matcher.Dumper.buildGraphviz (joinNode, [])

let betaDummy = mkBetaMemDummy [joinNode]

let betaGraph = Matcher.Dumper.buildGraphviz (betaDummy, [])