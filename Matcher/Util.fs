namespace Matcher

module Util =
    let rec update l k v =
        match l with
            [] -> [(k,v)]
            | (k',v)::l' -> if k = k' then (k',v)::l' else (k',v)::update l' k v

    let rec insertSet v vs =
        match vs with
            [] -> [v]
            | v' :: vs' -> if v = v' then vs else v'::insertSet v vs'

    let rec lookupOpt l k =
        match l with
            [] -> None
            | (k',v)::l' -> if k = k' then Some v else lookupOpt l' k            

    let rec updateMany l k v =
        match l with
            [] -> [(k,[v])]
            | (k',vs')::l' -> if k = k' then (k,insertSet v vs')::l' else (k',vs')::updateMany l' k v

    let single [x] = x

    let rec mapPartial f l =
        match l with
            [] -> []
            | x :: xs ->
                match f x with
                    None -> mapPartial f xs
                    | Some v -> v :: mapPartial f xs
    
    type order = LESS | EQUAL | GREATER

    let compareOrder x y = (function | -1 -> LESS | 0 -> EQUAL | 1 -> GREATER ) <| compare x y 
    
    let binarySearch (assocArray:array<'key * 'value>) (key:'key) =
        let rec binS min max =
            if max < min 
                then None
                else
                    let middle = (max - min) / 2 + min
                    let (middleKey,middleVal) = assocArray.[middle]
                    match compareOrder key middleKey with
                        LESS -> binS min (middle - 1)
                        | EQUAL -> Some middleVal
                        | GREATER -> binS (middle + 1) max
        binS 0 (assocArray.Length-1)

    let consAll v lists = Seq.map (fun list -> v :: list) lists

    let generateCombinations n insts =
        let rec g n =
            if n = 0 then
                Seq.singleton []
            else
                let vs = g (n-1)
                Seq.concat <| seq { for x in insts do yield consAll x vs }
        if n < 0 then
            failwith "cannot generate: negative count given"
        else
            g n