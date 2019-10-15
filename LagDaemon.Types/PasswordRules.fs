namespace LagDaemon.Types

type PasswordRules = {
    minLength: int 
    maxLength: int 
    requiredCharSets: RequiredChars list     
    minStrength: float
}
and RequiredChars = {
    minCount: int
    charSet: char list
}


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module PasswordRules =
    open System

    let upperCase = ['A'..'Z'] 
    let lowerCase = ['a'..'z']
    let punctuation = ['!'..'/'] @ [':'..'@'] @ ['['..'`'] @ ['~']
    let digits = ['0'..'9']

    let allCharSets = [upperCase; lowerCase; punctuation; digits]

    let defaultRule = {
        minLength = 8
        maxLength = 16
        minStrength = 28.0
        requiredCharSets = [
            {
                minCount = 2
                charSet = upperCase 
            }
            {
                minCount = 2
                charSet = lowerCase
            }
            {
                minCount = 2
                charSet = punctuation
            }
            {
                minCount = 2
                charSet = digits
            }

        ]
    } 

    let empty = {
        minLength = 0
        maxLength = Int32.MaxValue
        minStrength = 0.0
        requiredCharSets = []
    }
       
    let getMinLength rule = rule.minLength
    let getMaxLength rule = rule.maxLength
    let getCharSet rule = rule.requiredCharSets
    let getMinStrength rule = rule.minStrength

    let getMinCount charSet = charSet.minCount
    let getChars charSet = charSet.charSet
    let getAllChars rule =
        let charSet = getCharSet rule
        let rec inner rest accum =
            match rest with
            | [] -> accum
            | head::tail -> inner tail  (head.charSet @ accum)
        inner charSet []


    let setMinLength length rule =
        {
            rule with minLength = length
        }

    let setMaxLength length rule =
        {
            rule with maxLength = length
        }

    let addCharSet minCount charSet rule = 
        { rule with requiredCharSets =
                        ([
                            {
                                minCount = minCount
                                charSet = charSet
                            }
                         ] @ rule.requiredCharSets)
        }

    