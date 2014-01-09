#r @"bin\Debug\CoreLib.dll"
#r @"bin\Debug\FParsecCS.dll"
#r @"bin\Debug\FParsec.dll"

open FParsec

#load "ProdLang.fs"
#load "Parser.fs"
open Matcher.Parser
open CoreLib.Util
#load "Printer.fs"
open Matcher.Printer



let testParser p str =
    match run p str with
    | Success(result, _, _)   -> result
//    | Failure ->

let testProdsString = @"
    O.X = 1 and O.Y = 2 and O.Z = 3 >> P1
    O.X = 1 and O.Y = 2 and O.V = 4 and O.U = 5 >> P2
    O.X = 1 and O.Y = 2 and O.V = 4 and O.Z = 3 >> P3
    true >> P5
"


let p = run Matcher.Parser.prods testProdsString


let parsedProds = testParser prods testProdsString


let printedProds = printSystem parsedProds
let _ = System.Console.WriteLine(printedProds)
