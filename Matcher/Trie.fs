namespace Matcher

module Trie =
    type trie<'a,'b> = Trie of 'b list * (('a * trie<'a,'b>)list)

    let rec lookupTrie (Trie(values,tries)) kl =
        match kl with
            [] -> Some values
            | (k'::kl') -> 
                match Util.lookupOpt tries k' with
                    None -> None
                    | Some trie -> lookupTrie trie kl'

    let rec insert (Trie(values,tries)) (kl, v) =
        match kl with
            [] -> Trie(Util.insertSet v values,tries)
            | (k'::kl') -> 
                let rec insertTries subTries =
                    match subTries with
                        [] -> [(k',insert (Trie([],[])) (kl', v) )]
                        | (subK,trie) :: sts -> if subK = k' then (subK,insert trie (kl', v))::sts else (subK,trie) :: insertTries sts
                Trie(values, insertTries tries)

    let buildTrie assocList = Seq.fold insert (Trie([],[])) assocList