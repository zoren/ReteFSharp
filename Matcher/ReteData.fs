namespace Matcher

module ReteData =
    type symbol = string
    type WME = { fields : symbol * symbol * symbol }

    type token = WME list

    type fieldOfArg = Identifier | Attribute | Value

    type testAtJoinNode = 
        {
            fieldOfArg1 : fieldOfArg
            conditionNumberOfArg2 : int
            fieldOfArg2 : fieldOfArg
        }

    type BetaMemory = { items : token list ref }

    type NodeType = 
        Beta of BetaMemory
        | Join of joinData
        | Production of string * (token * WME) list ref
    and joinData =
        {
            amem : alphaMemory option ref
            tests : testAtJoinNode list
        }        
    and reteNode =
        {
            nodeType : NodeType
            children : reteNode list
            parent : reteNode option ref
        }
    and alphaMemory =
        {
            items : WME list ref
            successors : reteNode list
        }

    let rec map f { nodeType = nodeType; children = children } =
        f nodeType (List.map (map f) children)

    let rec iter f { nodeType = nodeType; children = children } =
        f nodeType ;List.iter (iter f) children
    // construction helpers
    let mkNullParent () = ref None

    let mkRete nodeType children = {nodeType = nodeType;children = children;parent = mkNullParent ()}
     
    let mkProd s = mkRete (Production (s, ref [])) []
    
    let mkTest (farg1,cond,farg2) = { fieldOfArg1 = farg1;conditionNumberOfArg2 = cond; fieldOfArg2 = farg2 }

    let mkNullAlpha () = ref None
    let mkJoin tests children = mkRete (Join {tests = tests;amem = mkNullAlpha ()}) children
    
    let mkBetaMem children = mkRete (Beta {items = ref []}) children
    let mkBetaMemDummy children = mkRete (Beta {items = ref [[]]}) children

    let mkAlphaMem children = {items = ref [];successors = children}   

    // backpointer helpers
    let rec setParents node =
        for child in node.children do
            child.parent := Some node
            setParents child

    let setAlphaMem alphaMem =
        for succ in alphaMem.successors do
            match succ.nodeType with
                Join jd -> jd.amem := Some alphaMem
                | _ -> ()

    let setBackPointers (reteTopNode, alphas) =
      setParents reteTopNode
      List.iter (fun (_,a) -> setAlphaMem a) alphas

    let resetBackPointers (reteTopNode, alphas) =
      let rec resetParents node =
        for child in node.children do
          child.parent := None
          resetParents child

      let resetAlphaMem alphaMem =
        for succ in alphaMem.successors do
            match succ.nodeType with
                Join jd -> jd.amem := None
                | _ -> ()
      resetParents reteTopNode
      List.iter (fun (_,a)-> resetAlphaMem a) alphas
