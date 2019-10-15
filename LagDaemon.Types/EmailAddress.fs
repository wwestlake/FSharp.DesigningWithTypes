namespace LagDaemon.Types

type EmailAddress = 
    | RawEmailAddress of string
    | ValidEmailAddress of ValidEmailAddress
    | InvalidEmailAddress of string
and ValidEmailAddress =
    | UnverifiedEmailAddress of string
    | VerifiedEmailAddress of string



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EmailAddress =

    open System.Text.RegularExpressions


    let create (s:string) =
        RawEmailAddress s

    let map rawfunc invalidfunc unverifiedfunc verifiedfunc e : EmailAddress =
        match e with
        | RawEmailAddress s -> rawfunc e
        | InvalidEmailAddress s -> invalidfunc e
        | ValidEmailAddress ve -> 
            match ve with
            | UnverifiedEmailAddress s -> unverifiedfunc e
            | VerifiedEmailAddress s -> verifiedfunc e

    let id x : EmailAddress = x
       

    let value e = 
        match e with
        | RawEmailAddress s -> s
        | InvalidEmailAddress s -> s
        | ValidEmailAddress ve -> 
            match ve with
            | UnverifiedEmailAddress s -> s
            | VerifiedEmailAddress s -> s

        
                    

    let apply f e =
        match e with
        | RawEmailAddress s -> f s
        | InvalidEmailAddress s -> f s
        | ValidEmailAddress ve -> 
            match ve with
            | UnverifiedEmailAddress s -> f s
            | VerifiedEmailAddress s -> f s




    let validate (ea: EmailAddress) =
        let rawfunc e =
            let s = value ea
            if Regex.IsMatch(s, @"^\S+@\S+\.\S+$")
            then ValidEmailAddress (UnverifiedEmailAddress s)
            else InvalidEmailAddress s


        map rawfunc id id id ea



    let createAndValidate = create >> validate

