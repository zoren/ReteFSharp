namespace MatcherTest

module ExampleProds =
    open ProdLang0.ProdLang

    let X = ("O","X")
    let C1 = Eq(X,"1")
    let Y = ("O","Y")
    let C2 = Eq(Y,"2")
    let Z = ("O","Z")
    let C3 = Eq(Z,"3")
    let V = ("O","V")
    let C4 = Eq(V,"4")
    let U = ("O","U")
    let C5 = Eq(U,"5")

    let prod1 = ([C1;C2;C3],"P1")
    let prod2 = ([C1;C2;C4;C5],"P2")
    let prod3 = ([C1;C2;C4;C3],"P3")

    //let testProds = [prod1;prod2;prod3]
    let testProds = 
        [ //Eq (("P", "K"),"6"); Eq (("P", "B"),"4"); 
           ([Eq (("O", "A"),"5")], "P0");
           ([Eq (("P", "A"),"5")], "P2")] //; Eq (("S", "A"),"6"); Eq (("O", "A"),"4")
    let testSys = {productions = testProds}

