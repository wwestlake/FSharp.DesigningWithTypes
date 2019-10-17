module LagDaemon.EmailTests

open System
open System.Text.RegularExpressions
open LagDaemon.Types
open NUnit.Framework
open FsCheck
open FsCheck.NUnit


type TestValidEmail = TestValidEmail of string                        
type TestInvalidEmail = TestInvalidEmail of string


let charListFromString str =
    Seq.toList str

let upperCase = ['A'..'Z']
let lowerCase = ['a'..'z']
let digits    = ['0'..'9']
let special_1 = "._%+-" |> charListFromString
let special_2 = ".-" |> charListFromString

let firstPart = upperCase @ lowerCase @ digits @ special_1
let secondPart = upperCase @ lowerCase @ digits @ special_2
let thirdPart = upperCase @ lowerCase


let random =
    let rand = Random(int DateTime.Now.Ticks)
    (fun min max -> rand.Next(min, max))

let randomFromList (xs: 'a list) =
    xs.[random 0 (List.length xs - 1)]

let randomListFromList (xs: 'a list) count =
    [for i in 0 .. count do randomFromList xs]

let listToString list = 
    list |> List.toArray |> System.String

let generateValidEmail valid =
    let validate ea =
        let regex = @"^[a-zA-Z0-9][a-zA-Z0-9._%+-]{0,63}@(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,62}[a-zA-Z0-9])?\.){1,8}[a-zA-Z]{2,63}$"
        Regex.IsMatch(ea, regex)

    let user () = randomListFromList firstPart (random 5 10)
    let domain () = randomListFromList secondPart (random 5 10)
    let topLevel () = randomListFromList thirdPart (random 2 10)
    let dot = ['.']
    let at = ['@']

    let rec create () count =
        let e = (user ()) @ dot @ (user ()) 
                @ at @ (domain ()) @ dot @ (domain ()) @ dot @ (topLevel ())
                |> listToString
        if (validate e) = valid then e 
        else if count <= 0 then e
        else create() (count - 1)

    create() 100

let validEmailList =
    [ for i in 0..1000 do yield (generateValidEmail true |> TestValidEmail) ]

let invalidEmailList =
    [ for i in 0..1000 do yield (generateValidEmail false |> TestInvalidEmail) ]



   

let chooseFromList xs = 
                  gen { let! i = Gen.choose (0, List.length xs-1) 
                        return List.item i xs }



type Generators () =
    static member ValidEmailList() =
        { new Arbitrary<TestValidEmail>() with
            override x.Generator = (chooseFromList validEmailList) }

    static member InvalidEmailList() =
        { new Arbitrary<TestInvalidEmail>() with
            override x.Generator = (chooseFromList invalidEmailList) }


[<SetUp>]
let Setup () =
    Arb.register<Generators>() |> ignore
    ()



[<Property>]
let ``EmailAddress creates a raw email address from a string`` (TestValidEmail email) =
    let e = EmailAddress.create email
    printfn "%A" e
    EmailAddress.applyRaw (fun x -> true) (fun _ -> false) e

[<Property>]
let ``EmailAddress validates a raw email address`` (TestValidEmail email) =
    let e = EmailAddress.create email
    let ve = EmailAddress.validate e
    printfn "%A" ve
    EmailAddress.applyUnverified (fun x -> true) (fun _ -> false) ve
    
[<Property>]
let ``EmailAddress invalidates a raw invalid email address`` (TestInvalidEmail email) =
    let e = EmailAddress.create email
    let ve = EmailAddress.validate e
    printfn "%A" ve
    EmailAddress.applyInvalid (fun x -> true) (fun _ -> false) ve







