#r @"bin\\Debug\CoreLib.dll"
#r @"bin\\Debug\Matcher.dll"
#r @"bin\\Debug\FsCheck.dll"

#load "Check.fs"
open FsCheck
open MatcherTest.Check

open Matcher.ProdLang

//let RevRevTree (xs:list<Tree>) = if xs = [Leaf 1] then false else List.rev(List.rev xs) = xs
//Check.Quick RevRevTree
//let prods = Arb.from<Production list>
//
//Arb.fromGen condition

//register()
//let RevRevBox (xs:System) = 
//    true
//    |> Prop.collect xs
//
//Check.Quick RevRevBox
//Check.Quick( (Prop.forAll <| ) RevRevBox)
//let ConditionPredicate (xs:ObjectVar list) = 
//    true
//    |> Prop.collect xs
//
//Check.Quick ConditionPredicate

//let generatedTree = Gen.eval 1 (Random.newSeed()) tree
//let generatedTrees = Gen.sample 1 3 tree
//let checkReport sys = referenceAndReteAgree sys |> Prop.collect sys
//Check.Quick checkReport


// Check.Quick referenceAndReteAgree
Check.Verbose referenceAndReteAgree

//let prods = [([Eq (("O", "A"),"3"); Eq (("P", "B"),"1"); Eq (("Q", "B"),"2");
//                 Eq (("O", "A"),"7")], "P0");
//               ([Eq (("P", "B"),"5"); TRUE; Eq (("Q", "B"),"4"); Eq (("Q", "E"),"5")], "P1");
//               ([Eq (("O", "A"),"3"); Eq (("P", "B"),"1"); Eq (("O", "A"),"2");
//                 Eq (("O", "A"),"7")], "P2");
//               ([Eq (("O", "A"),"5"); Eq (("O", "A"),"6"); Eq (("Q", "B"),"4");
//                 Eq (("O", "A"),"5")], "P3")]
//let filteredProds = Seq.map (fun(conds,prodName) -> (List.filter ((<>)TRUE) conds, prodName)) prods
//
//let sys = 
//    {productions =
//      [([Eq (("O", "A"),"4"); Eq (("P", "D"),"2"); Eq (("P", "C"),"4");
//         Eq (("O", "C"),"2"); Eq (("P", "A"),"5"); Eq (("O", "A"),"5")], "P0");
//       ([Eq (("P", "B"),"5"); Eq (("P", "C"),"7"); Eq (("O", "C"),"6");
//         Eq (("P", "C"),"2"); Eq (("O", "C"),"3"); Eq (("S", "A"),"3")], "P1");
//       ([Eq (("Q", "A"),"3")], "P2");
//       ([Eq (("P", "A"),"7"); Eq (("P", "A"),"6"); Eq (("P", "A"),"4")], "P3");
//       ([Eq (("O", "B"),"2"); Eq (("O", "A"),"2"); Eq (("O", "B"),"4");
//         Eq (("Q", "A"),"4"); Eq (("O", "B"),"7"); Eq (("O", "B"),"1")], "P4");
//       ([Eq (("O", "A"),"4"); Eq (("O", "A"),"4"); Eq (("R", "A"),"1");
//         Eq (("O", "C"),"4")], "P5");
//       ([Eq (("O", "C"),"7"); Eq (("R", "A"),"1"); Eq (("S", "A"),"6");
//         Eq (("O", "A"),"6")], "P6")];}
////let t0  = referenceAndReteAgree sys
//let sample n gn  = 
//   let rec sample i seed samples =
//       if i = 0 then samples
//       else sample (i-1) (Random.stdSplit seed |> snd) (Gen.eval 1000 seed gn :: samples)
//   sample n (Random.newSeed()) []
//
//let sampleN n gn = Gen.eval n (Random.newSeed()) gn
//
//let env = ref []
//
//let assign (inst,var,value) = env := (inst,var,value)::!env
//
//let tryLookup env key = List.tryFind (fun (k',_) -> key = k') env
//
//let randomInst() = 2
//
//let rec getAssignments localEnv conds =
//    match conds with
//        [] -> None
//        | (obj,var,value) :: cs -> 
//            match tryLookup localEnv obj with
//                Some (_,inst) -> 
//                    match List.tryFind (fun ((einst,evar),evalue) -> einst = inst && var = evar && evalue = value) (!env) with
//                        Some _ -> getAssignments ((randomInst(),var,value)::localEnv) cs
//                        | None -> 
//                | None -> 
//
