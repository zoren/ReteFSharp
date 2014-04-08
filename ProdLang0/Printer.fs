namespace ProdLang0

module Printer =
    open CoreLib
    open ProdLang0.ProdLang
    let printSystem ({productions = prods} as system)=
        let sb = new System.Text.StringBuilder()
        let app (s:string) = ignore <| sb.Append(s)

        let pexp = function |ExpValue value -> value.ToString() | ExpVariable var -> var

        let pcond c =
            match c with
                TRUE -> Seq.singleton "true "
                | Eq((o,var),exp) -> seq [o;".";var;" = "; pexp exp; " "]

        let pconds conds = Util.seqJoin "and " (Seq.map pcond conds)

        let pprod (conds,prodName) = Seq.append (pconds conds) (seq [">> " ;prodName])

        let strings = Util.seqJoin "\n" (Seq.map pprod prods)
        ((Seq.iter app strings);sb.ToString())
