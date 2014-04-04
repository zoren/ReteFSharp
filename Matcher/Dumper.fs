namespace Matcher

open Matcher.ReteData
open System.Collections.Generic

module Dumper =
    type graphNode = GMem of string
                    | GJoin
                    | GProd of string * string
                    | GAlpha of string
                    | GAlphaRoot
    open CoreLib
    
    let buildGraphviz (dumpGraph, alphaConds) =
 
        let count = ref 0
        let getIndex () = 
            let i = !count
            count := 1 + !count;i

        let dict = new Dictionary<reteNode, int>()

        let edges = ref []
        let nodes = ref []    

        let emitNode graphNode =
            let index = getIndex()
            nodes := (index,graphNode) :: !nodes
            index

        let emitEdgeLabel fromIndex toIndex label = edges := (fromIndex, toIndex, label)::!edges
        let emitEdge fromIndex toIndex = emitEdgeLabel fromIndex toIndex ""

        let wmeToString {fields =(inst,var,value)} = "(" + inst + " " + var + " " + value + ")"
        let tokenToString (token:token) = String.concat "::" <| List.map wmeToString token
        let tokensToString tokens = String.concat ", " <| List.map tokenToString tokens
        let justificationsToString justs = String.concat "" <| List.map (fun (token,wme) -> "{"+(tokenToString token)+":"+(wmeToString wme)+"}\l") justs

        let rec dumpRete (node:reteNode) =
            let index = match node.nodeType with
                            Beta bm -> emitNode <| GMem (tokensToString (!bm.items))
                            | Join jd -> emitNode GJoin
                            | Production (s,justification) -> emitNode <| GProd (s,justificationsToString !justification)
            let _ = dict.Add(node,index)
            let _ = for child in node.children do
                        emitEdge index (dumpRete child)
            index

        let dumpAlphaMems alphaConds =
            let alphaRootIndex = emitNode GAlphaRoot
            for ((var:string,value), alphaMem) in alphaConds do
              let alphaLabel = String.concat ", " <| List.map wmeToString (!alphaMem.items)
              let alphaIndex = emitNode (GAlpha alphaLabel)

              emitEdgeLabel alphaRootIndex alphaIndex (var + " = " + value)
              for child in alphaMem.successors do
                  let reteIndex = dict.[child]
                  emitEdge alphaIndex reteIndex
            alphaRootIndex
        (dumpRete dumpGraph,dumpAlphaMems alphaConds,!edges,!nodes)

    // conversion to dot format
    let nodeTypeToString nodeType =
        match nodeType with
            GMem label->  "[shape=box,label=\"" + label + "\"];"
            | GJoin -> "[shape=circle,label=\"\"];"
            | GProd (name,content) -> "[shape=box,style=rounded,xlabel=\"" + name + "\",label=\"" + content + "\"];"
            | GAlpha label -> " [shape= box,style=\"rounded,filled\",label=\"" + label + "\"]"
            | GAlphaRoot -> " [shape= box,style=\"rounded,filled\",label=\"Alpha\"]"

    let dumpNodes nodes =
        let indexedNode (index:int,node) = "\"" + index.ToString() + "\" " + nodeTypeToString node
        String.concat "\n" <| ( (List.map indexedNode nodes))

    let dumpEdges edges = 
        let dumpEdge (fromIndex:int,toIndex:int,label) = "\"" + fromIndex.ToString() + "\" -> \"" + toIndex.ToString() + "\"" + "[label=\"" + label + "\"]"
        String.concat "\n" <| Seq.map dumpEdge edges

    let isAlpha = function | GAlpha _ | GAlphaRoot -> true | _ -> false
    let isBeta n = not <| isAlpha n
    
    let dumpGraph ((dummyIndex:int,alphaRootIndex:int,edges,nodes), graphLabel) =
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

        let (alphaEdges,betaEdges) = List.partition (fun(_,toNode,_)->indexIsAlpha toNode) edges

        let alphaCluster = prefixAlpha::dumpNodes alphaNodes:: dumpEdges alphaEdges::"}"::[]

        let isProduction = function |(i, GProd _)-> Some (i.ToString()) | _ -> None
        let productionNodes = Util.mapPartial isProduction nodes
        let rankProds = "{rank=same; " + (String.concat ";" productionNodes) + ";}"

        let rankTopNodes = "{rank=same; " + dummyIndex.ToString() + ";" + alphaRootIndex.ToString() +  ";}"

        let betaCluster = prefixBeta::dumpNodes betaNodes:: dumpEdges betaEdges::"}"::[]
        let graphLabelDot = "label=\"" + graphLabel + "\""
        String.concat "\n" <| "digraph {":: alphaCluster @ rankProds :: rankTopNodes :: betaCluster @ graphLabelDot :: "}"::[]

    let dumpToFile graph path = System.IO.File.WriteAllText(path, dumpGraph graph)
