namespace MatcherTest

module Check =
    open ProdLang0.ProdLang
    open FsCheck
    open MatcherTest.ReteTester

    type Tree = Leaf of int | Branch of Tree * Tree

    let tree =
        let rec tree' s =
            match s with
            | 0 -> Gen.map Leaf Arb.generate<int>
            | n when n>0 ->
                let subtree = tree' (n/2)
                Gen.oneof [ Gen.map Leaf Arb.generate<int>
                            Gen.map2 (fun x y -> Branch (x,y)) subtree subtree]
            | _ -> invalidArg "s" "Only positive arguments are allowed"
        Gen.sized tree'

    type MyGenerators =
        static member Tree() =
            {new Arbitrary<Tree>() with
                override x.Generator = tree
                override x.Shrinker t = Seq.empty }
    let regi = Arb.register<MyGenerators>()

    let objectVariable =
        Gen.frequency <|
            Seq.map (fun(freq,v)->(freq, Gen.constant v))[(50,"O");(25,"P");(12,"Q");(5,"R");(5,"S");(2,"T")]
    let variable =
        Gen.frequency <|
            Seq.map (fun(freq,v)->(freq, Gen.constant v))[(50,"A");(25,"B");(12,"C");(5,"D");(5,"E");(2,"F")]

    let objVar : Gen<ObjectVar> = Gen.map2 (fun obj value -> (obj, value)) objectVariable variable

    let valueGen : Gen<string> = Gen.elements <| Seq.map (fun i -> (i.ToString()))[1;2;3;4;5;6;7]
//        Gen.frequency <|
//            Seq.map (fun(freq,v)->(freq, Gen.constant v))[(50,"1");(25,"2");(12,"3");(5,"4");(5,"5");(2,"6")]
    let expGen : Gen<ConditionExpression> = Gen.frequency [(3,Gen.map ExpValue valueGen);(1,Gen.map ExpVariable variable)]
    let eqCondition = Gen.map2 (fun obj value -> Eq(obj, value)) objVar expGen
    let condition = Gen.frequency [(98,eqCondition)] //todo  ;(2,Gen.constant TRUE)]
    //let conditions = Gen.listOf condition
    let conditions = Gen.sized (fun size -> Gen.nonEmptyListOf condition)

    let listOfConditions = Gen.sized (fun size -> Gen.resize ((size|>float|>sqrt|>int)) (Gen.listOf conditions))

    //let listOfConditions = Gen.sized (fun size -> Gen.resize (size|>float|>sqrt|>int) (Gen.listOf conditions))
//    let listOfAverageLength avgLength elemGen =
//        let lengthGen = Gen.choose (0,avgLength * 2)
//        Gen.map (fun gn n -> Gen.listOfLength n gn) lengthGen
    //Gen.sized (fun size -> Gen.resize (size|>float|>sqrt|>int) (Gen.listOf conditions))
//    let production = Gen.map2 (fun conds prodName -> (conds, prodName)) conditions Arb.generate<string>
//    let productions = Gen.listOf production

    let system =
        let f condsList = {productions =List.mapi (fun i conds -> (conds,"P" + i.ToString()) :Production) condsList}
        f <!> listOfConditions
    //let
//
    type ProdLangGenerators =
        static member Tree() =
            {new Arbitrary<System>() with
                override x.Generator = system
                override x.Shrinker t = Seq.empty }

    let _ = Arb.register<ProdLangGenerators>()

    let getComparisonFromCond cond =
        match cond with
            Eq((obj,var),ExpValue value) -> Seq.singleton (var,value)
            | TRUE -> Seq.empty

    open CoreLib

    let getObjVarsFromConds trie conds =
        let comparisons = Seq.collect getComparisonFromCond conds
        Seq.fold (fun s (var,value) -> Trie.insert s (Seq.toList var, value) ) trie comparisons

    let getVarsFromProds (prods: Production list) = Seq.fold (fun s (conds,_) -> getObjVarsFromConds s conds) Trie.empty prods

    let getVarValueAssocList prods =
        List.map (fun(kl:char list,vl)->(new string(List.toArray kl),vl))(Trie.toAssocList <| getVarsFromProds prods)

    let pickRandomFromList list =
        let length = List.length list
        let rnd = System.Random()
        let index = rnd.Next(length)
        List.nth list index

    let pickRandomFromSeq seq =
        let length = Seq.length seq
        if length = 0
        then
            None
        else
            let rnd = System.Random()
            let index = rnd.Next(length)
            Some(Seq.nth index seq)

    let pickRandomFromRange lower upper =
        let rnd = System.Random()
        rnd.Next(lower,upper)

    let flattenAssocList assocList = List.collect (fun(k,vs)-> List.map (fun v -> (k,v)) vs) assocList

    let getObjVarsFromCond cond =
        match cond with
            Eq((obj,var),value) -> Seq.singleton obj
            | TRUE -> Seq.empty

    let getObjVarsFromProductions (prods:seq<Production>) = Seq.distinct <| Seq.collect (fun (conds,_) -> Seq.collect getObjVarsFromCond conds) prods

    open Matcher
    open ProdLang0.MultiInstEvaluator

    let referenceAndReteAgree { productions = prods } =
        let numberOfProds = List.length prods
        let domain = flattenAssocList <| getVarValueAssocList prods

        // setup rete
        let (reteDummy, alphas) = ProdLang0.ReteBuilder.buildReteFromProductions prods
        let _ = Matcher.ReteData.setBackPointers (reteDummy, alphas)
        let assignReteInt (inst:int) = activateCond alphas ("$" + inst.ToString())

        // setup classic
        let mySys = mkSystem prods
        let myEnv = mkState mySys
        let myInstance = getNextInstance myEnv

        let assignReference inst var value = assignState (myEnv,inst) var value

        let normalizeState state = List.sort <| List.map (fun (prodName,states) -> (prodName, List.sort states)) state

        let assignBoth inst (var:Variable) (value:Value) = (assignReteInt inst var value;assignReference inst var value)

        let objvars = getObjVarsFromProductions prods

        let getProduction (prods:Production seq) prodName = Seq.tryFind (fun (_,n) -> n = prodName) prods

        //let myEnv = !myEnv.env
        let condSatisfied c =
            match c with
                Eq((_,var),ExpValue value) when List.exists (fun ((_,var'),value')-> var = var' && value = value') (!myEnv.env) -> true
                | TRUE -> true
                | _ -> false

        let getRandomAssignment (cl: Condition list) =
            let getUnsatisfied cl = List.filter (fun c -> not (condSatisfied c)) cl
            let myC = getUnsatisfied cl
            let c = pickRandomFromSeq myC
            let getVarValueFromCond cond =
                match cond with
                    Eq((_,var),value) -> Some (var,value)
                    | _ -> None
            Option.bind getVarValueFromCond c

        let numberOfObjectVars = Seq.length objvars

        let tryLookup env key = List.tryFind (fun (k',_) -> key = k') env

        let rec getAssign localEnv cl =
            match cl with
                  [] -> None
                | (Eq((obj,var), ExpValue value)) :: cl' ->
                    match tryLookup localEnv obj with
                        Some (_,prevInst) ->
                            match List.tryFind (fun ((einst,evar),evalue) -> einst = prevInst && var = evar && evalue = value) (!myEnv.env) with
                                Some ((einst,evar),evalue) -> getAssign ((obj,prevInst)::localEnv) cl'
                                | None -> Some (prevInst,var,value)
                        | None ->
                            let inst = System.Random().Next(0, numberOfObjectVars)
                            Some(inst,var,value)
//                    match List.tryFind (fun ((einst,evar),evalue) -> var = evar && evalue = value) (!myEnv.env) with
//                        Some ((einst,evar),evalue) -> getAssign ((obj,einst)::localEnv) cl'
//                        | None ->
//                            match List.tryFind (fun (obj',_) -> obj = obj') localEnv with
//                                Some (_,prevInst) -> Some(prevInst,var,value)
//                                | None ->
//                                    let inst = System.Random().Next(1, numberOfObjectVars)
//                                    Some(inst,var,value)

        let assignRandom (deactiveProdStates:ProductionsState) =

            let assignNew = System.Random().Next(0,2) = 0
//            if true
//                then
//                    let prodState = pickRandomFromSeq deactiveProdStates
//                    let (cl,_) = Option.get <| Option.bind (fun (prodName,_) -> getProduction prods prodName) prodState
//                    match getRandomAssignment cl with
//                        Some (var,value) -> assignBoth inst var value
//                        | None ->
//                            //System.Console.WriteLine("could not find deactive")
//                            let (var,value) = pickRandomFromList domain
//                            assignBoth inst var value
//                else
//                    let (var,value) = pickRandomFromList domain
            let prodState = pickRandomFromSeq deactiveProdStates
            let (cl,_) = Option.get <| Option.bind (fun (prodName,_) -> getProduction prods prodName) prodState
            match getAssign [] cl with
                Some (inst,var,value) ->
                    assignBoth inst var value
                | None ->
                    let inst = System.Random().Next(0, numberOfObjectVars)
                    let (var,value) = pickRandomFromList domain
                    assignBoth inst var value
//            System.Console.WriteLine("{0}.{1} <- {2}",inst,var,value)

        let rec activateAllProds iteration =
            let referenceState = normalizeState <| evalStateList myEnv
            let reteState = normalizeState <| getProductionNodes reteDummy

            let (activeRefProds, deactiveProds) = List.partition (fun (prodName,l) -> not <| List.isEmpty l ) referenceState
            let activeReteProds = List.filter (fun (prodName,l) -> not <| List.isEmpty l ) reteState

            if activeRefProds = activeReteProds then
                if List.length activeRefProds = numberOfProds then
                    ()
                else
                //System.Console.Write(iteration.ToString()+ " ");
                    (assignRandom deactiveProds;activateAllProds(iteration+1))
            else
                failwith <| "NOT EQUAL ref" + referenceState.ToString() + " rete " + reteState.ToString()
        let activate (var,value) =
            let referenceState = normalizeState <| evalStateList myEnv
            let reteState = normalizeState <| getProductionNodes reteDummy

            let (activeRefProds, deactiveProds) = List.partition (fun (prodName,l) -> not <| List.isEmpty l ) referenceState
            let activeReteProds = List.filter (fun (prodName,l) -> not <| List.isEmpty l ) reteState

            if activeRefProds = activeReteProds then
                if List.length activeRefProds = numberOfProds then
                    ()
                else
                    let inst = Util.rand numberOfObjectVars
                    assignBoth inst var value
            else
                failwith <| "NOT EQUAL ref" + referenceState.ToString() + " rete " + reteState.ToString()
        let checkCompleteness () =
            let referenceState = normalizeState <| evalStateList myEnv
            let reteState = normalizeState <| getProductionNodes reteDummy

            let (activeRefProds, deactiveProds) = List.partition (fun (prodName,l) -> not <| List.isEmpty l ) referenceState
            let activeReteProds = List.filter (fun (prodName,l) -> not <| List.isEmpty l ) reteState
            activeRefProds = activeReteProds && List.length activeRefProds = numberOfProds

        let rec activateAll n =
            if checkCompleteness() then
                n
            else
                Seq.iter activate (Util.randPermutateArray <| List.toArray domain);activateAll (n+1)

        System.Console.WriteLine("prods " + (List.length prods).ToString()+ " objVars " +  (Seq.length objvars).ToString())
        let sw = System.Diagnostics.Stopwatch.StartNew()
        //activateAllProds 0
        //Seq.iter activate (Util.randPermutateArray <| List.toArray domain)
        let activationIterations = activateAll 0
        if checkCompleteness() then
            ()
        else
            failwith "Not complete"
        System.Console.WriteLine("Elapsed: " + sw.ElapsedMilliseconds.ToString())
        System.Console.WriteLine("Activations: " + activationIterations.ToString())
        System.Console.WriteLine("env size: " + (List.length (!myEnv.env)).ToString())
        System.Console.WriteLine()
