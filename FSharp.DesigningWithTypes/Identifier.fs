namespace DesigningWithTypes


module Identifier =
    open Result

    type GUID = GUID of System.Guid
        with
            static member create () =
                GUID <| System.Guid.NewGuid()
            static member createFromGuid guid =
                GUID <| guid
            member __.value 
                with get() = let (GUID guid) = __ in guid
            member __.toStirng 
                with get() = let (GUID guid) = __ in guid.ToString()
            static member fromString (str) =
                let (flag, guid) = System.Guid.TryParse(str)
                if flag
                then GUID.createFromGuid guid |> Ok
                else Error "Guid is not properly formatted"

                    


    let newIdentifier () =
        GUID.create()

    let createFromGuid guid =
        GUID.createFromGuid guid

    let value (id:GUID) = id.value

    let toString (id:GUID) = id.ToString()
    
    let fromString str = GUID.fromString(str)


    