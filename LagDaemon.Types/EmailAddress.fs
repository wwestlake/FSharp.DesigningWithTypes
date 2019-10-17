namespace LagDaemon.Types



type EmailAddress =
    private
    | RawEmailAddress of string
    | ValidEmail of string * Token
    | InvalidEmail of string
    | VerifiedEmailAddress of string
    | FailedEmailAddress of string
and Token = Token of string




[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EmailAddress =
    open System
    open System.Text.RegularExpressions


    let create (str:string) = RawEmailAddress str



    let regex = @"^[a-zA-Z0-9][a-zA-Z0-9._%+-]{0,63}@(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,62}[a-zA-Z0-9])?\.){1,8}[a-zA-Z]{2,63}$"

    let createToken () =
        let rand = new Random(int DateTime.Now.Ticks)
        let tokenVals = ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9']
        let max = List.length tokenVals - 1
        let rec make acc count =
            if count > 0 then make (tokenVals.[rand.Next(max)] :: acc) (count - 1)
            else acc
        (make [] 4) @ ['-'] @ (make [] 4) @ ['-'] @ (make [] 4) @ ['-'] @ (make [] 4)
        |> List.toArray |> String.Concat |> Token

    let createTokenFromString s = Token s

    let internal select valid invalid verified failed x =
        match x with
        | ValidEmail (s,t) -> valid (s,t)
        | InvalidEmail s -> invalid s
        | VerifiedEmailAddress s -> verified s
        | FailedEmailAddress s -> failed s


    let validate em =
        match em with
        | RawEmailAddress s -> 
            match Regex.IsMatch(s, regex) with
            | true -> ValidEmail (s, createToken ())
            | false -> InvalidEmail s
        | InvalidEmail s -> FailedEmailAddress s
        | ValidEmail _ | VerifiedEmailAddress _ | FailedEmailAddress _ -> em


    let verify em (tok:Token) =
        match em with
        | ValidEmail (s,t) -> let (Token issued) = t in
                              let (Token supplied) = tok in
                              if issued = supplied then VerifiedEmailAddress s
                              else FailedEmailAddress s
        | InvalidEmail s -> FailedEmailAddress s
        | RawEmailAddress _ | VerifiedEmailAddress _ | FailedEmailAddress _ -> em


            
    




