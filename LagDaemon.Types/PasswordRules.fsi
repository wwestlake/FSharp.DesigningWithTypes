namespace LagDaemon.Types

type PasswordRules 
and RequiredChars

module PasswordRules =

    val upperCase : char list 
    val lowerCase : char list 
    val punctuation : char list 
    val digits : char list
    val allCharSets : char list list
    val defaultRule : PasswordRules 
    val getAllChars : rule:PasswordRules -> char list
    val empty : PasswordRules
    val getMinLength : rule:PasswordRules -> int
    val getMaxLength : rule:PasswordRules -> int
    val getCharSet : rule:PasswordRules -> RequiredChars list
    val getMinCount : charSet:RequiredChars -> int
    val getChars : charSet:RequiredChars -> char list
    val getMinStrength: rule:PasswordRules -> float
    val setMinLength : length:int -> rule:PasswordRules -> PasswordRules
    val setMaxLength : length:int -> rule:PasswordRules -> PasswordRules
    val addCharSet :  minCount:int -> charSet:char list -> rule:PasswordRules -> PasswordRules
    


