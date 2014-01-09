namespace MatcherTest

module SimpleExample =
    open Matcher.ReteData
    open Matcher.SimpleBuilder

    let mkWme f = { fields = f }

    let example = [(Var "x", Val "on", Var "y");
                    (Var "y", Val "left-of", Var "z");
                    (Var "z", Val "color", Val "red")]
                        
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

    