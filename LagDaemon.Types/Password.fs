namespace LagDaemon.Types

type Password =
    | ClearPassword of string
    | ValidClearPassword of string
    | HashedPassword of PasswordHash
    | InvalidPassword of string * string


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Password =

    open System
    open System.Security.Cryptography

    let create pw =
        ClearPassword pw

    let apply private f pw =
        match pw with
        | ClearPassword strpw -> f strpw
        | ValidClearPassword strpw -> f strpw
        | HashedPassword hashedpw -> f <| PasswordHash.value hashedpw
        | InvalidPassword (invalid, _) -> f invalid

    let map private fclear fvalid fhashed finvalid pw : Password =
        match pw with
        | ClearPassword strpw -> fclear strpw
        | ValidClearPassword strpw -> fvalid strpw
        | HashedPassword hashedpw -> fhashed hashedpw
        | InvalidPassword (invalid,_) -> finvalid invalid
       
    let private clearIdent strpw = ClearPassword strpw
    let private validIdent strpw = ValidClearPassword strpw
    let private hashedIdent strpw = HashedPassword strpw
    let private invalidIdent strpw = InvalidPassword (strpw, String.Empty)

    let mapClear f = map f validIdent hashedIdent invalidIdent
    let mapValid f = map clearIdent f hashedIdent invalidIdent
    let mapHashed f = map clearIdent validIdent f invalidIdent
    let mapInvalid f = map clearIdent validIdent hashedIdent f

    let usesCharset (pw:string) charSet =
        let rec inner rest result =
            match rest with
            | [] -> result
            | head :: tail -> 
                if (List.contains head charSet)
                then List.length charSet
                else inner tail 0
        inner (pw |> Seq.toList) 0
    
    

    let charSetSize password sets =
        List.map (usesCharset password) sets |> List.sum


    let entropy (password:string) =
        let R = charSetSize password PasswordRules.allCharSets
        let L = Seq.length password
        Math.Log2( Math.Pow( float R, float L ) ) 

    let private _validate f pw =
        map f 
            (fun pw -> ValidClearPassword pw) 
            (fun pw -> HashedPassword pw)   
            (fun pw -> InvalidPassword (pw, ""))
            pw

    let hashPassword pw =
        match pw with
        | ClearPassword clearpw -> ()
        | ValidClearPassword validpw -> ()
        | HashedPassword hashedpw -> ()
        | InvalidPassword (invalidpw, _) -> ()

    let validate  (rule:PasswordRules) (pw:Password) =
        let lenRule (str:string) =
            if str.Length < PasswordRules.getMinLength rule
            then InvalidPassword (str, "Too short")
            else if str.Length > PasswordRules.getMaxLength rule
            then InvalidPassword (str, "Too long")
            else ValidClearPassword str
                      
        
        let charSetRule (str:string) = 
            let pwList = Seq.toList str                 
            
            let rec charSetLoop rest result =
                let rec loop (count:int) (rest: char list) (charSet: char list) =
                    match rest with
                    | [] -> count
                    | head::tail -> 
                        if List.contains head charSet 
                        then loop (count + 1) tail charSet
                        else loop count tail charSet

                match rest with
                | [] -> result
                | head::tail -> 
                    charSetLoop tail ( result && ((loop 0 pwList (PasswordRules.getChars head)) >= PasswordRules.getMinCount head))


            if (charSetLoop (PasswordRules.getCharSet rule) true) 
                && (entropy str > PasswordRules.getMinStrength rule)
            then ValidClearPassword str
            else InvalidPassword (str, "Char Set")


        match pw with
        | ClearPassword strpw ->
            match _validate lenRule pw, _validate charSetRule pw with
            | ValidClearPassword _, ValidClearPassword str -> ValidClearPassword str
            | _,_ -> InvalidPassword (strpw, "Failed Validation")
        | _ -> pw
    


    // string -> Password
    let processPassword rules strpw  =
        strpw |> create |> (validate rules >> mapValid (PasswordHash.uberHash >> HashedPassword))


    let processPasswordDefault = processPassword PasswordRules.defaultRule
    let processPasswordNoRules = processPassword PasswordRules.empty


