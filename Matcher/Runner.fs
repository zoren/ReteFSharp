namespace Matcher

open ReteData

module Runner =
    open CoreLib
    let pickSymbol (wme:WME) fieldOfArg = 
        let (id,att,value) = wme.fields
        match fieldOfArg with
            Identifier -> id
            | Attribute -> att
            | Value -> value

    let wrt (s:string) = ()//System.Console.WriteLine(s)
    let wrt2 (s:string) = System.Console.WriteLine(s)

    let rec performJoinTests (tests:testAtJoinNode list, t: token, w:WME)=
        match tests with
            [] -> true
            | thisTest :: test' ->
                let arg1 = pickSymbol w thisTest.fieldOfArg1
                let wme2 = List.nth t thisTest.conditionNumberOfArg2
                let arg2 = pickSymbol wme2 thisTest.fieldOfArg2
                if arg1 <> arg2 then false else performJoinTests (test', t, w)
    
    let rec joinNodeRightActivation({nodeType = Join jd} as node :reteNode,w:WME) = 
        wrt "joinRight"
        if Option.isNone !node.parent then
            wrt "joinRightEq"
            //for child in node.children do leftActivation(child,[],w)//todo
        else
            wrt "joinRightNeq"
            let (Some({nodeType = Beta bm} as betaNode)) = !node.parent
            let items = !bm.items
            wrt ("joinRightNeq" + items.ToString())
            for t in items do
                if performJoinTests(jd.tests, t,w) then
                    for child in node.children do 
                    leftActivation(child, t, w)
                else ()
    
    and joinNodeLeftActivation({nodeType = Join jd} as node :reteNode,t:token) =
        wrt "joinLeft"
        let alphaMem = Option.get !jd.amem
        for w in !alphaMem.items do
            if performJoinTests (jd.tests, t, w) then
                for child in node.children do
                    leftActivation(child, t, w)
            else ()
                
    and betaMemoryLeftActivation ({nodeType = Beta bm} as node :reteNode,t:token,w:WME) =
        wrt "betaLeft"
        let newToken : token = w::t
        bm.items := newToken :: !bm.items
        for child in node.children do
            leftActivation(child, newToken, w)// there is a bug in Doorenboos here
    
    and rigthActivation(node,w) =
        wrt "right"
//        if obj.ReferenceEquals( nullRete, node) then
//            ()
//        else
        match node with
            {nodeType = Beta _} -> failwith "cannot rigth activate beta memories"
            | {nodeType = Join _} -> joinNodeRightActivation(node, w)
            | {nodeType = Production _} -> failwith "cannot rigth activate production"

    and leftActivation(node,t,w) = 
        wrt "left"
        match node with
              {nodeType = Beta _} -> betaMemoryLeftActivation (node, t, w)
            | {nodeType = Join _} -> joinNodeLeftActivation (node, t)
            | {nodeType = Production (s,matches)} ->
                let pp {fields = (o,vr,vl)} = "(" + o + "|" + vr + "|" + vl + ")"
//                let dump = wrt2 ("Left activate " + s + " with token: " + System.String.Join(", ", Seq.map pp t) + " and wme " + pp w)
                matches := (t,w) :: !matches
    
    and alphaMemoryActivation (node:alphaMemory, w:WME) = 
        node.items := w :: !node.items
        for child in node.successors do
            rigthActivation (child, w)
    
    let activateCond alphas inst variable value =
        let tup = (inst,variable,value)
        let wme = { fields = tup }
        match Util.lookupOpt alphas (variable,value) with
            Some alphaMem -> 
                if List.exists ((=)wme) !alphaMem.items then 
                    ()
                else            
                    alphaMemoryActivation(alphaMem, wme)
            | None -> ()

    let rec getProductionNodes ({nodeType = nodeType;children = children} as node) : (string * (int * string * string) list list) list =
        let deWME ({fields = (instString,var,value)}) = 
            let inst = System.Int32.Parse(instString.Substring(1))
            (inst,var,value)
        let deToken token = List.map deWME token
        let deMatch (token, wme) = (List.rev (deToken token)) @ [deWME wme]
        match nodeType with
            Production (prodName,matches) ->
                (prodName, List.map deMatch (!matches)) :: List.collect getProductionNodes children
            | _ -> List.collect getProductionNodes children

    