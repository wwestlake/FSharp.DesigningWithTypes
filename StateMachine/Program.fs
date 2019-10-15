// Learn more about F# at http://fsharp.org


open LagDaemon.Types


[<EntryPoint>]
let main argv =

    let passwords = [ 
        "2"
        "lame"
        "lamer"
        "KLJHkjhwaqihwqed987293784"
        "NotTooLameBuNotGood"
        "123456789"
        "Htrv9Gpl1234!@#$"
    ]

    printfn "%A" (List.map Password.processPasswordDefault passwords)

    System.Console.ReadKey(true) |> ignore

    0 // return an integer exit code


    
