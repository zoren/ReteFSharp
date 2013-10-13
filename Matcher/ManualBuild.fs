namespace Matcher

open ReteData

module ManualBuild =
    let mkProduction s = {nodeType = Production s;children = [];parent = ref nullRete}
    let mkBetaMemDummy children = {nodeType = Beta {items = ref [[]]};children = children;parent = ref nullRete}
    let mkBetaMem children = {nodeType = Beta {items = ref []};children = children;parent = ref nullRete}
    let mkJoin child tests = {nodeType = Join {tests = tests;amem = nullAlpha};children = [child];parent = ref nullRete}
    let mkJoinNT child = mkJoin child []

    let mkAlphaMem children = {items = ref [];successors = children}

    let resetNodeType (nt:NodeType) = 
        match nt with
            Beta bm -> bm.items := []
            | Join jd -> jd.amem.items := []
            | Production _ -> ()

    let rec resetReteNode (node:reteNode) =
        if obj.ReferenceEquals( nullRete, node) then
            ()
        else
            resetNodeType node.nodeType;List.iter resetReteNode node.children
    let resetAlpha (a:alphaMemory) = a.items := [];List.iter resetReteNode a.successors

    let P1 = mkProduction "P1"
    let P2 = mkProduction "P2"
    let P3 = mkProduction "P3"
    
    let join1 = mkJoinNT P1 //[{fieldOfArg1=Identifier;conditionNumberOfArg2 = 1;fieldOfArg2=Value}]
    let join2 = mkJoinNT P2
    let join3 = mkJoinNT P3
        
    let mC1C2C4 = mkBetaMem [P2;P3]

    let join4 = mkJoinNT mC1C2C4

    let mC1C2 = mkBetaMem [join1;join4]

    let join5 = mkJoinNT mC1C2

    let mC1 = mkBetaMem [join5]

    let join6 = mkJoinNT mC1

    let dummy = mkBetaMemDummy [join6]


    let alpha1 = mkAlphaMem [join6]

    let alpha2 = mkAlphaMem [join5]

    let alpha3 = mkAlphaMem [join1;join3]

    let alpha4 = mkAlphaMem [join4]

    let alpha5 = mkAlphaMem [join2]
    let alphas = [alpha1;alpha2;alpha3;alpha4;alpha5]
    let reset () = List.iter resetAlpha alphas
                  
    let mkVal var value = { fields = ("O",var,value) } :WME
    let _ = List.iter setAlphaMen alphas
    let _ = setParents dummy

    let activate alphaNode var value = alphaMemoryActivation(alphaNode, mkVal var value)
          
    let _ = activate alpha1 "X" "1"
    let _ = activate alpha2 "X" "1"
    let _ = activate alpha4 "X" "1"
    let _ = activate alpha5 "X" "1"

    let miniP1 = mkProduction "miniP1"
    let miniJoin = mkJoinNT miniP1
    let miniDummy = mkBetaMemDummy [miniJoin]

    let miniAlpha = mkAlphaMem [miniJoin]
    let _ = setParents miniDummy;;

