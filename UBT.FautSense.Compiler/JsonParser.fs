﻿namespace UBT.FaultSense.Compiler


module JsonParser =
    open FParsec
    open Model


    type Json = 
        | JString of string
        | JNumber of float
        | JBool   of bool
        | JNull
        | JList   of Json list
        | JObject of Map<string, Json>

    let jnull   : Parser<_> = stringReturn "null" JNull
    let jtrue   : Parser<_> = stringReturn "true" (JBool true)
    let jfalse  : Parser<_> = stringReturn "true" (JBool false)
    let jnumber : Parser<_> = pfloat |>> JNumber

    let str s = pstring s
    let ws = spaces

    let stringLiteral : Parser<_> =
        let escape =  anyOf "\"\\/bfnrt"
                      |>> function
                          | 'b' -> "\b"
                          | 'f' -> "\u000C"
                          | 'n' -> "\n"
                          | 'r' -> "\r"
                          | 't' -> "\t"
                          | c   -> string c // every other char is mapped to itself
    
        let unicodeEscape =
            /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9
    
            str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
            )
    
        let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')
    
        between (str "\"") (str "\"")
                (stringsSepBy normalCharSnippet escapedCharSnippet)
    
    let jstring = stringLiteral |>> JString

    let jvalue , jvalueRef = createParserForwardedToRef<Json, UserState>()

    let listBetweenStrings sOpen sClose pElement f =
        between (str sOpen) (str sClose)
                (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)

    let jlist   = listBetweenStrings "[" "]" jvalue JList

    let keyValue : Parser<_> = stringLiteral .>>. (ws >>. str ":" >>. ws >>. jvalue)

    let jobject : Parser<_> = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)


    do jvalueRef := choice [jobject
                            jlist
                            jstring
                            jnumber
                            jtrue
                            jfalse
                            jnull]

    let json = ws >>. jvalue .>> ws .>> eof



