module Eval

    type Value = int
    type Variable = string

    type Exp = Eq of Variable * Value
                | And of Exp * Exp

    type Env = (Variable * Value) list
    type AndList = (Variable * Value) list

    let rec lookup al k =
        match al with
            [] -> None
            | ((k',v')::_) when k = k' -> Some v'
            | (_::al') -> lookup al' k

    let andOpt v1 v2 = 
        match v1, v2 with
              Some true, Some true -> Some true
            | Some false, _ -> Some false
            | _, Some false -> Some false
            | _ -> None

    let lookupEquals (env:Env) var value = 
        match lookup env var with
            None -> None
            | Some value' -> Some (value = value')

    let rec eval env exp =
        match exp with
            Eq (var, value) -> lookupEquals env var value
            | And (e1, e2) ->
                andOpt (eval env e1) (eval env e2)

    let evalList env (expList:AndList) =
        List.fold (fun acc (var,value) -> andOpt acc (lookupEquals env var value)) (Some true) expList
