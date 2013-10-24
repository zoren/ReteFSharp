#r @"bin\\Debug\Matcher.dll"
#r @"bin\\Debug\FsCheck.dll"

#load "Check.fs"
open FsCheck
open MatcherTest.Check

open Matcher.ProdLang

//let RevRevTree (xs:list<Tree>) = if xs = [Leaf 1] then false else List.rev(List.rev xs) = xs
//Check.Quick RevRevTree
let prods = Arb.from<Production list>

let RevRevBox (xs:Production list) = 
    List.rev(List.rev xs) = xs
    |> Prop.collect xs

Check.Quick RevRevBox
//let generatedTree = Gen.eval 1 (Random.newSeed()) tree
//let generatedTrees = Gen.sample 1 3 tree