namespace Matcher

open Matcher.ReteData
open System.Collections.Generic

module Dumper =
    type graphNode = GMem of string
                    | GJoin of string
                    | GProd of string * string
                    | GAlpha of string
                    | GAlphaRoot
    open CoreLib

    let buildGraphviz (dumpGraph, alphaConds, activatedEdges) =
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

        let emitEdgeLabelColor fromIndex toIndex label color = edges := (fromIndex, toIndex, label, color)::!edges
        let emitEdgeLabel fromIndex toIndex label = emitEdgeLabelColor fromIndex toIndex label None
        let emitEdge fromIndex toIndex = emitEdgeLabelColor fromIndex toIndex "" None
        let emitEdgeWithNode (fromNode,fromIndex) (toNode,toIndex) =
          let color = if (List.exists ((=) (LeftActivation(fromNode,toNode))) activatedEdges) then
                        Some "red"
                      else
                        None
          emitEdgeLabelColor fromIndex toIndex "" color

        let emitAlphaEdgeWithNode (fromNode,fromIndex) (toNode,toIndex) =
          let color = if (List.exists ((=) (RigthActivation(fromNode,toNode))) activatedEdges) then
                        Some "red"
                      else
                        None
          emitEdgeLabelColor fromIndex toIndex "" color

        let wmeToString {fields =(inst,var,value)} = "(" + inst + " " + var + " " + value + ")"
        let tokenToString (token:token) =
          let rec tokensToString l =
            match l with
              [] -> ["[]"]
              | tok :: tokens -> wmeToString tok :: "::" :: tokensToString tokens
          String.concat "" <| tokensToString token
        let tokensToString tokens = String.concat ", " <| List.map tokenToString tokens
        let justificationsToString justs = String.concat "" <| List.map (fun (token,wme) -> "{"+(wmeToString wme)+":"+(tokenToString token)+"}\l") justs

        let fieldToString field =
          match field with
            Identifier -> "inst"
            | Attribute -> "attr"
            | Value -> "val"
        let testToString {fieldOfArg1 = f1;conditionNumberOfArg2 = condNum;fieldOfArg2 = f2} =
          "." + (fieldToString f1) + " = t[" + condNum.ToString()  + "]." + (fieldToString f2) + "\l"

        let rec dumpRete (node:reteNode) =
            let graphNode = match node.nodeType with
                              Beta bm -> GMem (tokensToString (!bm.items))
                              | Join jd -> GJoin <| String.concat "" (Seq.map testToString jd.tests)
                              | Production (s,justification) -> GProd (s,justificationsToString !justification)
            let index = emitNode graphNode
            dict.Add(node,index)
            for child in node.children do
                emitEdgeWithNode (node,index) (child,dumpRete child)
            index
        let patternToString pattern =
          match pattern with
            WMEAnyThing -> "*"
            | WMEValue v -> v
        let dumpAlphaMems alphaConds =
            let alphaRootIndex = emitNode GAlphaRoot
            for ((var:string,value), alphaMem) in alphaConds do
              let alphaLabel = String.concat ", " <| List.map wmeToString (!alphaMem.items)
              let alphaIndex = emitNode (GAlpha alphaLabel)

              emitEdgeLabel alphaRootIndex alphaIndex (var + " = " + (patternToString value))
              for child in alphaMem.successors do
                  let reteIndex = dict.[child]
                  emitAlphaEdgeWithNode (alphaMem,alphaIndex) (child,reteIndex)
            alphaRootIndex
        (dumpRete dumpGraph,dumpAlphaMems alphaConds,!edges,!nodes)

    // conversion to dot format
    let nodeTypeToString nodeType =
        match nodeType with
            GMem label->  "[shape=box,label=\"" + label + "\"];"
            | GJoin testString -> "[shape=circle,label=\"" + testString + "\"];"
            | GProd (name,content) -> "[shape=box,style=rounded,xlabel=\"" + name + "\",label=\"" + content + "\"];"
            | GAlpha label -> " [shape= box,style=\"rounded,filled\",label=\"" + label + "\"]"
            | GAlphaRoot -> " [shape= box,style=\"rounded,filled\",label=\"Alpha\"]"

    let dumpNodes nodes =
        let indexedNode (index:int,node) = "\"" + index.ToString() + "\" " + nodeTypeToString node
        String.concat "\n" <| ( (List.map indexedNode nodes))

    let dumpEdges edges =
        let dumpEdge (fromIndex:int,toIndex:int,label, colorOpt) =
          let colorString = match colorOpt with
                              Some color -> ",color=\"" + color + "\""
                              | None -> ""
          "\"" + fromIndex.ToString() + "\" -> \"" + toIndex.ToString() + "\"" + "[label=\"" + label + "\"" + colorString + "]"
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

        let (alphaEdges, betaEdges) = List.partition (fun(_:int,toNode,_,_) -> indexIsAlpha toNode) edges

        let alphaCluster = prefixAlpha::dumpNodes alphaNodes:: dumpEdges alphaEdges::"}"::[]

        let isProduction = function |(i, GProd _)-> Some (i.ToString()) | _ -> None
        let productionNodes = Util.mapPartial isProduction nodes
        let rankProds = "{rank=same; " + (String.concat ";" productionNodes) + ";}"

        let rankTopNodes = "{rank=same; " + dummyIndex.ToString() + ";" + alphaRootIndex.ToString() +  ";}"

        let betaCluster = prefixBeta::dumpNodes betaNodes:: dumpEdges betaEdges::"}"::[]
        let graphLabelDot = "label=\"" + graphLabel + "\""
        String.concat "\n" <| "digraph {":: alphaCluster @ rankProds :: rankTopNodes :: betaCluster @ graphLabelDot :: "}"::[]

    let dumpToFile graph path = System.IO.File.WriteAllText(path, dumpGraph graph)
