namespace LagDaemon.Types


type Identifier = Identifier of System.Guid

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Identifier =
    open Result

    type Identifier
        with
            static member create () =
                Identifier <| System.Guid.NewGuid()
            static member createFromGuid guid =
                Identifier <| guid
            member __.value 
                with get() = let (Identifier guid) = __ in guid
            member __.toStirng 
                with get() = let (Identifier guid) = __ in guid.ToString()
            static member fromString (str:string) =
                let (flag, guid) = System.Guid.TryParse(str)
                if flag
                then Identifier.createFromGuid guid |> Ok
                else Error "Guid is not properly formatted"

                    


    let create () =
        Identifier.create()

    let map id f =
        let (Identifier guid) = id in f guid



    let fromGuid guid =
        Identifier.createFromGuid guid

    let value (id:Identifier) = id.value

    let toString (id:Identifier) = id.ToString()
    
    let fromString str = Identifier.fromString(str)


    