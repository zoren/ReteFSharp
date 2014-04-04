namespace Matcher

open Matcher.ReteData

module Runner =
    open CoreLib
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
    
    let rec joinNodeRightActivation({nodeType = Join jd} as node :reteNode,w:WME) = 
        if Option.isNone !node.parent then
            ()
            //for child in node.children do leftActivation(child,[],w)//todo
        else
            let (Some({nodeType = Beta bm} as betaNode)) = !node.parent
            let items = !bm.items
            for t in items do
                if performJoinTests(jd.tests, t,w) then
                    for child in node.children do 
                    leftActivation(child, t, w)
                else ()
    
    and joinNodeLeftActivation({nodeType = Join jd} as node :reteNode,t:token) =
        let alphaMem = Option.get !jd.amem
        for w in !alphaMem.items do
            if performJoinTests (jd.tests, t, w) then
                for child in node.children do
                    leftActivation(child, t, w)
            else ()
                
    and betaMemoryLeftActivation ({nodeType = Beta bm} as node :reteNode,t:token,w:WME) =
        let newToken : token = w::t
        bm.items := newToken :: !bm.items
        for child in node.children do
            leftActivation(child, newToken, w)// there is a bug in Doorenboos here
    
    and rigthActivation(node,w) =
        match node with
            {nodeType = Beta _} -> failwith "cannot rigth activate beta memories"
            | {nodeType = Join _} -> joinNodeRightActivation(node, w)
            | {nodeType = Production _} -> failwith "cannot rigth activate production"

    and leftActivation(node,t,w) = 
        match node with
              {nodeType = Beta _} -> betaMemoryLeftActivation (node, t, w)
            | {nodeType = Join _} -> joinNodeLeftActivation (node, t)
            | {nodeType = Production (s,matches)} ->
                matches := (t,w) :: !matches
    
    and alphaMemoryActivation (node:alphaMemory, w:WME) = 
        node.items := w :: !node.items
        for child in node.successors do
            rigthActivation (child, w)

