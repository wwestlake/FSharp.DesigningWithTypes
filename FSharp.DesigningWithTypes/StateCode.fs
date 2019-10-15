namespace DesigningWithTypes

open System.IO



module StateCode = 

    open FSharp.Data

    type T = StateCode of string

    type State = { name: string; abbreviation: string }

    type StateProvider = JsonProvider<"https://gist.githubusercontent.com/mshafrir/2646763/raw/8b0dbb93521f5d6889502305335104218454c2bf/states_titlecase.json">

    let listStates () =
        [
            for state in StateProvider.GetSamples() do
                let name = state.Name
                let abbrev = state.Abbreviation
                yield { name = name; abbreviation = abbrev }
        ]

    let findState code =
        let list = listStates ()
        try
            Some (listStates() |> List.find (fun state -> state.abbreviation = code))
        with
            | exn -> None


    // create with continuation
    let createWithCont success failure  (s:string) = 
        let s' = s.ToUpper()
        let stateCode = findState s
        match stateCode with
        | Some state ->success (StateCode s') 
        | None -> failure "State is not in list"
    
    // create directly
    let create s = 
        let success e = Some e
        let failure _  = None
        createWithCont success failure s

    // unwrap with continuation
    let apply f (StateCode e) = f e

    // unwrap directly
    let value e = apply id e
