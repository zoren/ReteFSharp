namespace MatcherTest

module SimpleExample =
    open Matcher.ReteData
    open ProdLang0.SimpleBuilder

    let mkWme f = { fields = f }

    let example = [(Var "x", Val "on", Var "y");
                    (Var "y", Val "left-of", Var "z");
                    (Var "z", Val "color", Val "red")]

    let simpleExampleProds = [([(Var "x", Val "on", Var "y")],"P1");
                              ([(Var "x", Val "on", Var "y");(Var "y", Val "left-of", Var "z")],"P2");
                              (example, "P3");
                              (example@[(Var "z", Val "color", Val "blue")], "P4")]

    let B1 = "B1"
    let B2 = "B2"
    let B3 = "B3"
    let B4 = "B4"

    let on = "on"
    let color = "color"
    let leftOf = "left-of"

    let red = "red"
    let blue = "blue"
    let table = "table"
    
    let w1 = mkWme (B1,on,B2)
    let w2 = mkWme (B1,on,B3)
    let w3 = mkWme (B1,color,red)

    let w4 = mkWme (B2,on,table)
    let w5 = mkWme (B2,leftOf,B3)
    let w6 = mkWme (B2,color,blue)

    let w7 = mkWme (B3,leftOf,B4)
    let w8 = mkWme (B3,on,table)
    let w9 = mkWme (B3,color,red)

    let w10 = mkWme (B3,color,blue)


    let C1 = (Var "x", Val "on", Var "y")
    let C2 = (Var "y", Val "left-of", Var "z")
    let C3 = (Var "z", Val "color", Val "red")
    let C4 = (Var "zz", Val "color", Val "red")
    let C5 = (Var "zzz", Val "color", Val "red")

    let prod1 = ([C1;C2;C3],"P1")
    let prod2 = ([C1;C2;C4;C5],"P2")
    let prod3 = ([C1;C2;C4;C3],"P3")

    let simpleTestProds = [prod1;prod2;prod3]
