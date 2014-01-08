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

//    let nullRete : reteNode option = None
//    let nullAlpha : alphaMemory option = None
    let rec map f { nodeType = nodeType; children = children } =
        f nodeType (List.map (map f) children)

    let rec iter f { nodeType = nodeType; children = children } =
        f nodeType ;List.iter (iter f) children

    type testTree = TestTreeNode of (fieldOfArg * int * fieldOfArg) list * testTree list

    let deRecTest { fieldOfArg1 = farg1;conditionNumberOfArg2 = cond; fieldOfArg2 = farg2 } = (farg1,cond,farg2)

    let print nodeType children = 
        match nodeType with
            Join {tests = tests} -> TestTreeNode (List.map deRecTest tests, children )
            | _ -> TestTreeNode ([], children )
    let printTests node = map print node
    
