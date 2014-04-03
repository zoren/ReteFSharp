#r @"bin\Debug\CoreLib.dll"
#r @"bin\Debug\FParsecCS.dll"
#r @"bin\Debug\FParsec.dll"
#r @"bin\Debug\Matcher.dll"

open FParsec

#load "ProdLang.fs"
#load "Parser.fs"
open ProdLang0.Parser
open CoreLib.Util
#load "Printer.fs"
open ProdLang0.Printer



let testParser p str =
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(message,_,_) -> raise (new System.Exception(message))

let testProdsString = @"
    O.X = 1 and O.Y = 2 and O.Z = 3 >> P1
    O.X = 1 and O.Y = 2 and O.V = 4 and O.U = 5 >> P2
    O.X = 1 and O.Y = 2 and O.V = 4 and O.Z = 3 >> P3
    true >> P5
"


let p = run ProdLang0.Parser.prods testProdsString


let parsedProds = testParser prods testProdsString

let printedProds = printSystem parsedProds
let _ = System.Console.WriteLine(printedProds)

#load "ReteBuilder.fs"

let (rootnode, alphaMems) = ProdLang0.ReteBuilder.buildReteFromSystem parsedProds
let graph = Matcher.Dumper.buildGraphviz (rootnode, Seq.map (fun (_,mem) -> mem) alphaMems)

let _  = Matcher.Dumper.dumpToFile graph "graph.dot"
