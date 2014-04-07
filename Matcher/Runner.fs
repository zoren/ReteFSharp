namespace Matcher

open Matcher.ReteData
open CoreLib

type EdgeActivation = RigthActivation of alphaMemory * reteNode
                    | LeftActivation of reteNode * reteNode

type LoggingRunner (activate:EdgeActivation -> unit) =
    let pickSymbol (wme:WME) fieldOfArg =
        let (id,att,value) = wme.fields
        match fieldOfArg with
            Identifier -> id
            | Attribute -> att
            | Value -> value

    let rec performJoinTests (tests:testAtJoinNode list, t: token, w:WME)=
        match tests with
            [] -> true
            | thisTest :: test' ->
                let arg1 = pickSymbol w thisTest.fieldOfArg1
                let wme2 = List.nth t thisTest.conditionNumberOfArg2
                let arg2 = pickSymbol wme2 thisTest.fieldOfArg2
                if arg1 <> arg2 then false else performJoinTests (test', t, w)

    let rec joinNodeRightActivation({nodeType = Join jd} as node, w:WME) =
        let (Some({nodeType = Beta bm} as betaNode)) = !node.parent
        let items = !bm.items
        for t in items do
            if performJoinTests(jd.tests, t, w) then
                for child in node.children do
                leftActivation(node, child, t, w)
            else ()

    and joinNodeLeftActivation({nodeType = Join jd} as node, t:token) =
        let alphaMem = Option.get !jd.amem
        for w in !alphaMem.items do
            if performJoinTests (jd.tests, t, w) then
                for child in node.children do
                    leftActivation(node, child, t, w)
            else ()

    and betaMemoryLeftActivation ({nodeType = Beta bm} as node, t:token, w:WME) =
        let newToken : token = w::t
        bm.items := newToken :: !bm.items
        for child in node.children do
            leftActivation(node, child, newToken, w)// there is a bug in Doorenboos here

    and rigthActivation(node,w) =
        match node.nodeType with
              Beta _ -> failwith "cannot rigth activate beta memories"
            | Join _ -> joinNodeRightActivation(node, w)
            | Production _ -> failwith "cannot rigth activate production"

    and leftActivation(parent, node,t,w) =
        activate(LeftActivation(parent,node))
        match node.nodeType with
              Beta _ -> betaMemoryLeftActivation (node, t, w)
            | Join _ -> joinNodeLeftActivation (node, t)
            | Production (s, matches) -> matches := (t,w) :: !matches

    and alphaMemoryActivation (node:alphaMemory, w:WME) =
        node.items := w :: !node.items
        for child in node.successors do
            activate(RigthActivation(node, child))
            rigthActivation (child, w)

    member this.activateAlphaMemory (node:alphaMemory, inst, variable, value) =
        let tup = (inst,variable,value)
        let wme = { fields = tup }
        this.activateAlphaMemory(node, wme)

    member this.activateAlphaMemory (node:alphaMemory, wme) =
        alphaMemoryActivation(node, wme)

type Runner () = inherit LoggingRunner (fun _ -> ())