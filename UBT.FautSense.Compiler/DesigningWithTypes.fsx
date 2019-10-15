

type EmailAddress = EmailAddress of string
type ZipCode = ZipCode of string
type StateCode = StateCode of string


type PostalAddress = 
    {
    Address1: string;
    Address2: string;
    City: string;
    State: StateCode;
    Zip: ZipCode;
    }


type EmailContactInfo = 
    {
    EmailAddress: EmailAddress;
    IsEmailVerified: bool;
    }


type PostalContactInfo = 
    {
    Address: PostalAddress;
    IsAddressValid: bool;
    }


type PersonalName = 
    {
    FirstName: string;
    // use "option" to signal optionality
    MiddleInitial: string option;
    LastName: string;
    }

type Contact = 
    {
        Name: PersonalName
        EmailContactInfo: EmailContactInfo
        PostalContactInfo: PostalContactInfo
    }


// functions

let CreateEmailAddress success failure (s:string) = 
    if System.Text.RegularExpressions.Regex.IsMatch(s,@"^\S+@\S+\.\S+$") 
        then success (EmailAddress s)
        else failure "Email address must contain an @ sign"

let CreateStateCode success failure (s:string) = 
    let s' = s.ToUpper()
    let stateCodes = ["AZ";"CA";"NY"] //etc
    if stateCodes |> List.exists ((=) s')
        then success (StateCode s')
        else failure "Not a valud state code"


let handler value =
    printfn "%A" value

// test code

CreateStateCode handler handler "CA"
CreateStateCode handler handler "XX"

CreateEmailAddress handler handler "a@example.com"
CreateEmailAddress handler handler "example.com"






