namespace UBT.FaultSense.Compiler

module Examples =

    open UBT.FaultSense.Compiler
    open System
    open FParsec
    open Model


    let test p str =
        match run p str with
        | Success(result, state, s2)   -> printfn "Success: %A %A - %A" state s2 result
        | Failure(errorMsg, state, s2) -> printfn "Failure: %A %A - %s" state s2 errorMsg

    type StringConstant = StringConstant of string * string


    let examples () = 
        
        let str s = pstring s
        let ws = spaces
        let str_ws s = pstring s .>> ws
        let float_ws = pfloat .>> ws

        let betweenStrings s1 s2 p = p |> between (str s1) (str s2)
        
        let floatBetweenBrackets = pfloat |> betweenStrings "[" "]"

        let floatList = str "[" >>. sepBy pfloat (str ",") .>> str "]" 
        let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"
        let numberListFile = ws >>. numberList .>> eof
        //let identifier =
        //    let isIdentifierFirstChar c = isLetter c || c = '_'
        //    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        //
        //    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
        //    .>> ws // skips trailing whitespace
        
        let isAsciiIdStart c =
            isAsciiLetter c || c = '_'
        
        let isAsciiIdContinue c =
            isAsciiLetter c || isDigit c || c = '_' || c = '\''

        let identifier =
            let isIdentifierFirstChar c = isLetter c || c = '_'
            let isIdentifierChar c = isLetter c || isDigit c || c = '_'

            many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
            .>> ws // skips trailing whitespace

        let stringLiteral =
            let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
            let unescape c = match c with
                             | 'n' -> '\n'
                             | 'r' -> '\r'
                             | 't' -> '\t'
                             | c   -> c
            let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
            between (pstring "\"") (pstring "\"")
                    (manyChars (normalChar <|> escapedChar))
        
        let stringLiteral2 =
            let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
            let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                                    | 'n' -> "\n"
                                                                    | 'r' -> "\r"
                                                                    | 't' -> "\t"
                                                                    | c   -> string c)
            between (pstring "\"") (pstring "\"")
                    (manyStrings (normalCharSnippet <|> escapedChar))

        
        let stringLiteral3 =
            let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
            let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                                    | 'n' -> "\n"
                                                                    | 'r' -> "\r"
                                                                    | 't' -> "\t"
                                                                    | c   -> string c)
            between (pstring "\"") (pstring "\"")
                    (stringsSepBy normalCharSnippet escapedChar)
        

        let product = pipe2 float_ws (str_ws "*" >>. float_ws)
                            (fun x y -> x * y)



        let stringConstant = pipe3 identifier (str_ws "=") stringLiteral
                                (fun id _ str -> StringConstant(id, str))

        let boolean = (stringReturn "true" true) <|> (stringReturn "false" false)


        let pipe7 p1 p2 p3 p4 p5 p6 p7 f =
            pipe4 p1 p2 p3 (tuple4 p4 p5 p6 p7)
                  (fun x1 x2 x3 (x4, x5, x6, x7) -> f x1 x2 x3 x4 x5 x6 x7) 

        test (ws >>. (str "a" <|> str "b")) " b"
        test boolean "true"
        test boolean "false"
        test (pipe7 float_ws float_ws float_ws float_ws float_ws float_ws float_ws (fun a b c d e f g -> a + b + c + d + e + f + g)) "1 2 3 4 5 6 7"
        test (float_ws .>>. (str_ws "," >>. float_ws)) "123, 456"
        test stringConstant "_test = \"fortune\""
        test product "4 * 5"

        test stringLiteral "\"this is a test \n\t\r this is more testing\n\""
        test stringLiteral2 "\"this is a test \n\t\r this is more testing\n\""
        test stringLiteral3 "\"this is a test \n\t\r this is more testing\n\""
        test stringLiteral3 "\"\""
        test identifier "_test1="
        test identifier "_"
        test (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0"
        test (many (str "a" <|> str "b")) "abba"
        test floatList "[]"
        test floatList "[1,2,3,4.5,7.2]"
        test numberListFile @"[1 ,2 ,3,
                            4, 5, 6 ]"





