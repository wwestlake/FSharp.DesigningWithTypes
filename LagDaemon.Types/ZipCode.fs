namespace LagDaemon.Types



module ZipCode =

    type T = ZipCode of string

    // create with continuation
    let createWithCont success failure  (s:string) = 
        if System.Text.RegularExpressions.Regex.IsMatch(s,@"^\d{5}$") 
            then success (ZipCode s) 
            else failure "Zip code must be 5 digits"
    
    // create directly
    let create s = 
        let success e = Some e
        let failure _  = None
        createWithCont success failure s

    // unwrap with continuation
    let apply f (ZipCode e) = f e

    // unwrap directly
    let value e = apply id e

