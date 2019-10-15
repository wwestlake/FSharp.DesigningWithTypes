
module TypeStrings
    open System

    let rec foldr f z l =
        match l with
        | [] -> z
        | x::xs -> f x (foldr f z xs)

    [<AbstractClass>]
    type SafeStringBase(content : String) =
        member this.AsString with get() = content
        override this.ToString() = content
        
    // Now here #SafeStringBase really means SafeString of the corresponding 
    // concrete language type but there isn't a way to express that
    type ILanguage =
        abstract member LiteralFragment         : String -> #SafeStringBase // String is a literal language fragment
        abstract member LiteralText             : String -> #SafeStringBase // String is literal text
        abstract member NativeRepresentation    : #SafeStringBase -> String   // Gets the native-language representation
        abstract member Language                : unit -> String             // Gets the name of the language  
        abstract member Empty                   : unit -> #SafeStringBase     // creates an empty SafeString in the Language
        abstract member Add                     : #SafeStringBase -> #SafeStringBase -> #SafeStringBase
        
    type SafeString<'TLanguage when 'TLanguage :> ILanguage and 'TLanguage : (new : unit -> 'TLanguage) > (content : String) =
        inherit SafeStringBase(content)
        static let language : 'TLanguage = new 'TLanguage()
        
        // this is ugly
        static member private ToType (x:#SafeStringBase) =
            x :> SafeStringBase :?> SafeString<'TLanguage>
        
        static member Empty : SafeString<'TLanguage> =
            language.Empty() |> SafeString<'TLanguage>.ToType
        
        // takes a string that you certify as representing a fragment in the Language 
        // and returns a corresponding SafeString
        static member Fragment fragment =
            language.LiteralFragment fragment |> SafeString<'TLanguage>.ToType
        
        // takes a string that you certify as representing text and returns a 
        // corresponding SafeString in the Language
        static member Text text =
            language.LiteralText text |> SafeString<'TLanguage>.ToType
            
        static member Join (strings : seq<SafeString<'TLanguage>>) =
          let l = Seq.toList strings
          (foldr language.Add (language.Empty()) l) |> SafeString<'TLanguage>.ToType
        
        static member (+) ((self: SafeString<'TLanguage>), (other: SafeString<'TLanguage>)) : SafeString<'TLanguage> =
            (language.Add self other)  |> SafeString<'TLanguage>.ToType
            
          
    type Xml () =
      // this too is ugly
      static member private ToType (x:#SafeStringBase) =
            x :> SafeStringBase :?> SafeString<Xml>
      static member private ToHashType (x:#SafeStringBase) =
            x :> SafeStringBase :?> #SafeStringBase
            
      interface ILanguage with
        member self.LiteralFragment s = Xml.ToHashType (new SafeString<Xml>(s)) 
        member self.LiteralText s = Xml.ToHashType (new SafeString<Xml>(System.Security.SecurityElement.Escape(s)))
        member self.NativeRepresentation s =
            s.AsString.Replace("&apos;", "'").
              Replace("&quot;", "\"").Replace("&gt;", ">").
              Replace("&lt;", "<").Replace("&amp;", "&")
        member self.Language () = "XML"
        member self.Empty () = Xml.ToHashType (new SafeString<Xml>(String.Empty))
        
        // Don't think we can force this at compile time
        // The best we can do is encapsulate this operation
        member self.Add x y = 
            let left = Xml.ToType x
            let right = Xml.ToType y
            Xml.ToHashType (new SafeString<Xml>(x.AsString + y.AsString)) 
      
