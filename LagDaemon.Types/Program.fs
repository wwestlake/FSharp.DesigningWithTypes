namespace LagDaemon.Types

module Program =

    open System
    
    [<EntryPoint>]
    let main args =
        let em = EmailAddress.create "bill@email.com"
        printfn "%A" em
    
        let ve = EmailAddress.validate em
        printfn "%A" ve
    
        let print e =
            let printRaw (e:EmailAddress) = printfn "Raw email: %A" e ; e
            let printValid  (e:EmailAddress) = printfn "Valid : %A" e ; e
            
    
            let a = EmailAddress.map printRaw printValid EmailAddress.id EmailAddress.id e
            ()
    
        print em
        print ve
    
    
        Console.ReadKey(true);
        0