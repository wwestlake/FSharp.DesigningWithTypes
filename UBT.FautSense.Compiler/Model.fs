namespace UBT.FaultSense.Compiler



module Model =

    open FParsec

    type UserState = 
            {
                name: string
                symbols: Map<string, obj>
                children: UserState list
                parent: UserState option
            }
              with static member create (name) = {
                            name = name
                            symbols = Map.empty
                            children = []
                            parent = None
                          }
                   member x.addSymbol(name, value) = {
                                        x with
                                            symbols = Map.add name value (x.symbols)
                                        }
                   member x.addChild(newState) = {
                                        x with
                                            children = newState :: x.children
                                        }                
                   member x.createChild(parent, name) = 
                                        let newChild = 
                                            {
                                                name = name
                                                symbols = Map.empty
                                                children = []
                                                parent = Some parent
                                            }
                                        x.addChild(newChild) |> ignore
                                        newChild

                   member x.popState () = x.parent

    type Parser<'t> = Parser<'t, UserState>

    type Identifier = Identifier of string


    and ValueType =
        | Definition of  Identifier * ValueType
        | Sbyte      of sbyte
        | Short      of int16  
        | Int        of int
        | Byte       of byte
        | UShort     of uint16
        | UInt       of uint32
        | ULong      of uint64
        | Char       of char
        | Float      of float
        | Double     of double
        | Decimal    of decimal
        | Bool       of bool
        | Enum       of (Identifier * int) list
        | Struct     of (Identifier * Identifier list)

    and Assignment = Identifier * Expression

    and Condition = Expression

    and Statement =
        | ForLoop of Assignment * Expression * Expression
        | IfThenElse of Condition * StatementBlock * StatementBlock
        | While of Condition * StatementBlock

    and StatementBlock = Statement list
    
    and Expression = ASTNode * ASTNode

    and ASTNode =
        | Module of (Identifier list) * (ASTNode list)
        | Statement of Statement
        | Expression of Expression
        | Value of ValueType
        
    let parser, parserRef = createParserForwardedToRef<ASTNode, UserState>()

    // Keywords

    // types
    let kw_sbyte                 : Parser<_> = pstring "sbyte"
    let kw_short                 : Parser<_> = pstring "short"
    let kw_int                   : Parser<_> = pstring "int"
    let kw_byte                  : Parser<_> = pstring "byte"
    let kw_ushort                : Parser<_> = pstring "ushort"
    let kw_uint                  : Parser<_> = pstring "uint"
    let kw_ulong                 : Parser<_> = pstring "ulong"
    let kw_char                  : Parser<_> = pstring "char"
    let kw_float                 : Parser<_> = pstring "float"
    let kw_double                : Parser<_> = pstring "double"
    let kw_decimal               : Parser<_> = pstring "decimal"
    let kw_bool                  : Parser<_> = pstring "bool"
    let kw_enum                  : Parser<_> = pstring "enum"
    let kw_struct                : Parser<_> = pstring "struct"
                                 
    // control                   
    let kw_module                : Parser<_> = pstring "module"
    let kw_test                  : Parser<_> = pstring "test"
    let kw_for                   : Parser<_> = pstring "for"
    let kw_every                 : Parser<_> = pstring "every"
    let kw_if                    : Parser<_> = pstring "if"
    let kw_then                  : Parser<_> = pstring "then"
    let kw_else                  : Parser<_> = pstring "else"
    let kw_while                 : Parser<_> = pstring "while"
    let kw_do                    : Parser<_> = pstring "do"
    let kw_var                   : Parser<_> = pstring "var" 
    let kw_val                   : Parser<_> = pstring "val" 
    let kw_let                   : Parser<_> = pstring "let" 
    let kw_done                  : Parser<_> = pstring "done"
    let kw_all                   : Parser<_> = pstring "all"
    let kw_each                  : Parser<_> = pstring "each"
    let kw_expect                : Parser<_> = pstring "exprc"
    let kw_assert                : Parser<_> = pstring "assert"
    let kw_try                   : Parser<_> = pstring "try"
    let kw_catch                 : Parser<_> = pstring "catch"
    let kw_finally               : Parser<_> = pstring "finally"
    let kw_use                   : Parser<_> = pstring "use"
    let kw_fun                   : Parser<_> = pstring "fun"
    let kw_type                  : Parser<_> = pstring "type"
    

    // operators                 
                                 
    let op_add                   : Parser<_> = pstring "+"
    let op_incr                  : Parser<_> = pstring "++"
    let op_sub                   : Parser<_> = pstring "-"
    let op_decr                  : Parser<_> = pstring "--"
    let op_mul                   : Parser<_> = pstring "*"
    let op_div                   : Parser<_> = pstring "/"
    let op_pow                   : Parser<_> = pstring "^"
    let op_less_than             : Parser<_> = pstring "<"
    let op_greater_than          : Parser<_> = pstring ">"
    let op_less_than_or_eq       : Parser<_> = pstring "<="
    let op_greater_than_or_eq    : Parser<_> = pstring ">="
    let op_not_eq                : Parser<_> = pstring "<>"  
    let op_mod                   : Parser<_> = pstring "%"
    let op_and                   : Parser<_> = pstring "&&"
    let op_or                    : Parser<_> = pstring "||"
    let op_not                   : Parser<_> = pstring "!"
    let op_xor                   : Parser<_> = pstring "~"
    let op_bw_and                : Parser<_> = pstring "&&&"
    let op_bw_or                 : Parser<_> = pstring "|||"
    let op_bw_xor                : Parser<_> = pstring "^^^"
    let op_bw_neg                : Parser<_> = pstring "~~~"
    let op_bw_shift_left         : Parser<_> = pstring "<<<"
    let op_bw_shift_right        : Parser<_> = pstring ">>>"
                                 
    // groups                    
    let grp_left_paren           : Parser<_> = pstring "("  
    let grp_right_paren          : Parser<_> = pstring ")"  
    let grp_left_bracket         : Parser<_> = pstring "["   
    let grp_right_bracket        : Parser<_> = pstring "]"   
    let grp_left_brace           : Parser<_> = pstring "{"   
    let grp_right_brace          : Parser<_> = pstring "}"   

    let grp_strong_left_paren    : Parser<_> = pstring "(|"  
    let grp_strong_right_paren   : Parser<_> = pstring "|)"  
    let grp_strong_left_bracket  : Parser<_> = pstring "[|"   
    let grp_strong_right_bracket : Parser<_> = pstring "|]"   
    let grp_strong_left_brace    : Parser<_> = pstring "{|"   
    let grp_strong_right_brace   : Parser<_> = pstring "|}"   

    let grp_modi_left_paren      : Parser<_> = pstring "(*"  
    let grp_modi_right_paren     : Parser<_> = pstring "*)"  
    let grp_modi_left_bracket    : Parser<_> = pstring "[*"   
    let grp_modi_right_bracket   : Parser<_> = pstring "*]"   
    let grp_modi_left_brace      : Parser<_> = pstring "{*"   
    let grp_modi_right_brace     : Parser<_> = pstring "*}"   


    // functions              
    let right_arrow              : Parser<_> = pstring "->"
    let left_arrow               : Parser<_> = pstring "<-"
    let lambda                   : Parser<_> = pstring ",\\"
    
    // comments
    let line_comment             : Parser<_> = pstring "//"
    let block_comment_start      : Parser<_> = pstring "/*"
    let block_comment_end        : Parser<_> = pstring "*/"














