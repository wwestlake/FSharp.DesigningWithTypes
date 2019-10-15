namespace DesigningWithTypes

open System.IO



module ContactInfo = 

    open DesigningWithTypes

    type PersonalName =
        {
            FirstName: string
            MiddleInitial: string option
            LastName: string
        }
        with
            static member create first last (mi: string option) =
                {
                    FirstName = first
                    LastName = last
                    MiddleInitial = mi
                }

            member __.format () =
                match __.MiddleInitial with
                | Some mi -> sprintf "%s, %s %s" __.LastName __.FirstName mi
                | None    -> sprintf "%s, %s" __.LastName __.FirstName


                

    type EmailContactInfo =
        {
            EmailAddress: EmailAddress
            IsEmailVerified: bool
        }
        with 
            static member create email verified =
                {
                    EmailAddress = EmailAddress.create email
                    IsEmailVerified = verified
                }
    

    type PostalAddress =
        {
            Address1: string
            Address2: string option
            City: string
            State: StateCode.T
            Zip: ZipCode.T
        }
        with    
            static member create addr1 (addr2: string option) city state zip =
                let checkZip = ZipCode.create zip
                match checkZip with
                | Some z ->
                    match addr2 with
                         | Some a2 ->
                           Ok {
                               Address1 = addr1
                               Address2 = Some a2
                               City = city
                               State = state
                               Zip =  z
                           }
                         | None ->
                           Ok {
                               Address1 = addr1
                               Address2 = None
                               City = city
                               State = state
                               Zip =  z
                           }
                | None -> Result.Error "Zip code not properly formatted"

    type PostalContactInfo =
        {
            Address: PostalAddress
            IsAddressValid: bool
        }
        with 
            static member create addr1 (addr2: string option) city state zip isValid =
                match PostalAddress.create addr1 addr2 city state zip with
                | Ok addr -> 
                    Ok {
                        Address = addr
                        IsAddressValid = isValid
                    }
                | Error msg -> Error msg

    type ContactInfo = 
        | EmailOnly of EmailContactInfo
        | PostOnly of PostalContactInfo
        | EmailAndPost of EmailContactInfo * PostalContactInfo

    type PhoneContactInfo = PhoneContactInfo of string

    type ContactMethod = 
        | Email of EmailContactInfo 
        | PostalAddress of PostalContactInfo 
        | HomePhone of PhoneContactInfo 
        | WorkPhone of PhoneContactInfo 
   
 

    type Contact =
        {
            Name: PersonalName
            PrimaryContactMethod: ContactMethod
            SecondaryContactMethods: ContactMethod list
        }
            

    [<AutoOpen>]
    module ContactInfoHelper =

        open System





