// Learn more about F# at http://fsharp.org


open LagDaemon.Types
open System.Net



[<EntryPoint>]
let main argv =

    let result = TcpIp.connect "www.google.com" 80
    match result with
    | Ok conn -> printfn "%A" conn
                 printfn "%s" (conn.reader.ReadToEnd())
                 conn.writer.Write("GET / HTTP/1.1\n\r")
                 conn.writer.Flush()
                 printfn "%s" (conn.reader.ReadToEnd())

    | Error msg -> printfn "%A" msg

    0 // return an integer exit code


    
