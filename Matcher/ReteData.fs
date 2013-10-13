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
        | Production of string
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
