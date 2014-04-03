namespace Matcher

open Matcher.ReteData
open System.Collections.Generic

module Dumper =
    type graphNode = GMem | GJoin | GProd of string | GAlpha | GAlphaRoot
    open CoreLib
    
    let buildGraphviz (dumpGraph, alphaConds) =
 
        let count = ref 0
        let getIndex () = 
            let i = !count
            count := 1 + !count;i

        let dict = new Dictionary<reteNode, int>()

        let edges = ref []
        let nodes = ref []    

        let emitNode gt = 
            let index = getIndex()
            nodes := (index,gt) :: !nodes
            index

        let rec dumpRete (node:reteNode) =
            let index = match node.nodeType with
                            Beta bm -> emitNode GMem
                            | Join jd -> emitNode GJoin
                            | Production (s,_) -> emitNode <| GProd s
            let _ = dict.Add(node,index)
            let _ = for child in node.children do
                        edges := (index,dumpRete child)::!edges
            index
    
        let dumpAlphaMems alphaMems =
            let alphaRootIndex = emitNode GAlphaRoot
            for alphaMem in alphaMems do
                let alphaIndex = emitNode GAlpha
                edges := (alphaRootIndex,alphaIndex)::!edges
                for child in alphaMem.successors do
                    let reteIndex = dict.[child]
                    edges := (alphaIndex,reteIndex)::!edges
        (ignore <| dumpRete dumpGraph;dumpAlphaMems alphaConds;(!edges,!nodes))

    let nodeTypeToString nodeType =
        match nodeType with
            GMem ->  "[shape=box];"
            | GJoin -> "[shape=circle];"
            | GProd name -> "[shape=plaintext,label=\"" + name+"\"];"
            | GAlpha -> " [shape= box,style=\"rounded,filled\"]"
            | GAlphaRoot -> " [shape= box,style=\"rounded,filled\",label=\"Alpha\"]"

    let dumpNodes nodes =
        let indexedNode (index:int,node) = "\"" + index.ToString() + "\" " + nodeTypeToString node
        String.concat "\n" <| ( (List.map indexedNode nodes))

    let dumpEdges edges = 
        String.concat "\n" <| Seq.map (fun (fromIndex:int,toIndex:int) -> "\"" + fromIndex.ToString() + "\" -> \"" + toIndex.ToString() + "\"") edges
    
    let isAlpha = function | GAlpha | GAlphaRoot -> true | _ -> false
    let isBeta n = not <| isAlpha n
    
    let dumpGraph (edges,nodes) = 
        let prefixAlpha = @"
    subgraph cluster_0{
    rankdir=RL;
  	    color=invis;"
        let prefixBeta = @"
subgraph cluster_1{
rankdir=TB;
  	color=invis;"
        let postfixBeta = @"}"

        let alphaNodes = List.filter (fun(_,n)-> isAlpha n) nodes
        let betaNodes = List.filter (fun(_,n)-> isBeta n) nodes
        
        let indexIsAlpha index = List.exists (fun (i,_) -> i = index) alphaNodes

        let (alphaEdges,betaEdges) = List.partition (fun(fromNode,toNode)->indexIsAlpha toNode) edges

        let alphaCluster = prefixAlpha::dumpNodes alphaNodes:: dumpEdges alphaEdges::"}"::[]

        let isProduction = function |(i, GProd s)-> Some (i.ToString()) | _ -> None
        let productionNodes = Util.mapPartial isProduction nodes
        let rankProds = "{rank=same; " + (String.concat ";" productionNodes) + ";}"

        let betaCluster = prefixBeta::dumpNodes betaNodes:: dumpEdges betaEdges::"}"::[]
        String.concat "\n" <| "digraph {":: alphaCluster @ rankProds :: betaCluster @ "}"::[]

    let dumpToFile graph = System.IO.File.WriteAllText("graph.dot",dumpGraph graph)
