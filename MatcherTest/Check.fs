namespace MatcherTest

module Check = 
    open Matcher.ProdLang
    open FsCheck
    
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

    let objVar = Gen.map2 (fun obj value -> (obj, value)) objectVariable variable

    let value = 
        Gen.frequency <| 
            Seq.map (fun(freq,v)->(freq, Gen.constant v))[(50,"1");(25,"2");(12,"3");(5,"4");(5,"5");(2,"6")]
    let eqCondition = Gen.map2 (fun obj value -> Eq(obj, value)) objVar value
    let condition = Gen.frequency [(98,eqCondition);(2,Gen.constant TRUE)]
    let conditions = Gen.listOf condition
    let listOfConditions = Gen.listOf conditions
    let production = Gen.map2 (fun conds prodName -> (conds, prodName)) conditions Arb.generate<string>
    let productions = Gen.listOf production

    let prods2 =
        let f condsList = List.mapi (fun i conds -> (conds,"P" + i.ToString()) :Production) condsList
        f <!> listOfConditions
    //let 
//
    type ProdLangGenerators =
        static member Tree() =
            {new Arbitrary<Production list>() with
                override x.Generator = prods2
                override x.Shrinker t = Seq.empty }            

    let register = Arb.register<ProdLangGenerators>()