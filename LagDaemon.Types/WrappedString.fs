namespace LagDaemon.Types

type IWrappedString =
    abstract Value : string



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module WrappedString =




    let create canonicalize isValid ctor (s:string) =
        if s = null
        then None
        else
            let s' = canonicalize s
            if isValid s'
            then Some (ctor s')
            else None

    let apply f (s:IWrappedString) =
        s.Value |> f

    let value s = apply id s

    let equals left right =
        (value left) = (value right)

    let compareTo left right =
        (value left).CompareTo(value right)


    let singleLineTrimmed s =
        System.Text.RegularExpressions.Regex.Replace(s, "\s+", " ").Trim()

    let lengthValidator len (s:string) =
        s.Length <= len

    type String100 = String100 of string with
        interface IWrappedString with
            member this.Value = let (String100 s) = this in s


    let string100 = create singleLineTrimmed (lengthValidator 100) String100 

    /// Converts a wrapped string to a string of length 100
    let convertTo100 s = apply string100 s


    /// A string of length 50
    type String50 = String50 of string with
        interface IWrappedString with
            member this.Value = let (String50 s) = this in s

    /// A constructor for strings of length 50
    let string50 = create singleLineTrimmed (lengthValidator 50)  String50

    /// Converts a wrapped string to a string of length 50
    let convertTo50 s = apply string50 s

    type Text1000 = Text1000 of string with
        interface IWrappedString with
            member this.Value = let (Text1000 s) = this in s

    /// A constructor for multiline strings of length 1000
    let text1000 = create id (lengthValidator 1000) Text1000 


    /// map helpers
    let mapAdd k v map = 
        Map.add (value k) v map    

    let mapContainsKey k map =  
        Map.containsKey (value k) map    

    let mapTryFind k map =  
        Map.tryFind (value k) map    


