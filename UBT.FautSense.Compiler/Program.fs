
namespace UBT.FaultSense.Compiler

open System


module Main =


//    let test (p: Parser<_>) str =
//        match runParserOnString p (UserState.create "test" ) "test" str with
//        | Success(result, state, s2)   -> printfn "Success: %A %A - %A" state s2 result
//        | Failure(errorMsg, state, s2) -> printfn "Failure: %A %A - %s" state s2 errorMsg
//
//    let testFile (p:Parser<_>) filename =
//        match runParserOnFile p (UserState.create "test" ) filename (System.Text.Encoding.UTF8) with
//        | Success(result, state, s2)   -> printfn "Success: %A %A - %A" state s2 result
//        | Failure(errorMsg, state, s2) -> printfn "Failure: %A %A - %s" state s2 errorMsg



    
    let check = EmailAddress.create "test@test.com"
    


    [<EntryPoint>]
    let main arg =

        match StateCode.create "CA" with
        | Some code -> StateCode.apply (printfn "%s") code
        | None -> printfn "Error---"

        
        


        Console.ReadKey(true) |> ignore
        0 // return an integer exit code


