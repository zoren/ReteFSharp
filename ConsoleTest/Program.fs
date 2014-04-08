open FParsec
open System.IO

let testParser p str =
  match run p str with
  | Success(result, _, _)   -> result
  | Failure(message,_,_) -> raise (new System.Exception(message))

let generatePNGfromDot dotFilePath pngFilePath =
  System.Diagnostics.Process.Start(@"C:\Program Files (x86)\Graphviz2.36\bin\dot.exe", dotFilePath + @" -Tpng -o " + pngFilePath)

let generateWithAssignments ((rootnode, alphaMems) as graph) assignments =
    Matcher.ReteData.setBackPointers graph
    let activatedEdges = ref []
    let loggingRunner = new Matcher.LoggingRunner(fun activatedEdge -> activatedEdges := activatedEdge :: !activatedEdges)

    for (inst, var, value) in assignments do
      match MatcherTest.ReteTester.lookupAlphaMem alphaMems var value with
        Some alphaMem ->
                let tup = (inst,var,value)
                let wme = { Matcher.ReteData.fields = tup }
                if List.exists ((=)wme) !alphaMem.items then
                    ()
                else
                    loggingRunner.activateAlphaMemory(alphaMem, inst, var, value)
        | None -> raise (System.ArgumentException("No alpha memory matches assignment."))
    Matcher.ReteData.resetBackPointers graph
    Matcher.Dumper.buildGraphviz (rootnode, alphaMems, !activatedEdges)

let assignmentsToString assignments = String.concat "\l" <| Seq.map (fun(inst,var,value) -> inst + "." + var + " := " + value) assignments

let saveGraphWithAssignments filePath graph assignments (testProdsString:string) =
    let graph = generateWithAssignments graph assignments

    let currentFileName = filePath + Seq.length(assignments).ToString()
    let graphFileName = currentFileName + ".dot"
    let pngFileName = currentFileName + ".png"

    let productionsLineBreakedForDot = String.concat "" <| (Seq.map (fun l -> l + "\l") (testProdsString.Split('\n')))
    let graphLabel = productionsLineBreakedForDot + "\lWith assignments:\l" + assignmentsToString assignments+ "\l"

    Matcher.Dumper.dumpToFile (graph, graphLabel) graphFileName
    generatePNGfromDot graphFileName pngFileName

let getAllPrefixes list = seq {for i in 0 .. (Seq.length list) -> Seq.toList <| Seq.take i list }

let parseAssignment (ass:string) =
  match Array.map (fun (s:string) -> s.Trim() ) <| ass.Split([|":=";"."|], System.StringSplitOptions.RemoveEmptyEntries) with
    [|inst;var;value|] -> (inst,var,value)
    | x -> raise (System.Exception())

let parseAssignments (str:string) = Seq.map parseAssignment (str.Split([|";";"\n"|], System.StringSplitOptions.RemoveEmptyEntries))

let generateAndDumpReteGraphFromProductions (filePath:string) (assignmentFilePath:string) =
    let testProdsString = File.ReadAllText filePath

    let parsedProds = testParser ProdLang0.Parser.prods testProdsString

    let (rootnode, alphaMems) as graph = ProdLang0.ReteBuilder.buildReteFromSystem parsedProds

    let assignmentString = File.ReadAllText assignmentFilePath
    let assignments = parseAssignments assignmentString
    let filePathNoExt = Path.GetFileNameWithoutExtension filePath

    for assignmentSubSeq in getAllPrefixes assignments do
      saveGraphWithAssignments filePathNoExt graph assignmentSubSeq testProdsString

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    let _ = match argv with
              [|productionFileName;assignmentFilePath|] -> generateAndDumpReteGraphFromProductions productionFileName assignmentFilePath
              | _ -> raise (new System.Exception(""))
    0 // return an integer exit code
