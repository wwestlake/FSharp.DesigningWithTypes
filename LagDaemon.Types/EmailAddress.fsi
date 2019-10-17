namespace LagDaemon.Types

type EmailAddress
and Token = Token of string


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EmailAddress =

    val create : str:string -> EmailAddress
    val internal select :
      valid:(string * Token -> 'a) ->
        invalid:(string -> 'a) ->
          verified:(string -> 'a) ->
            failed:(string -> 'a) -> x:EmailAddress -> 'a
    val validate : em:EmailAddress -> EmailAddress
    val verify : em:EmailAddress -> tok:Token -> EmailAddress
    val createTokenFromString : s:string -> Token
    
    

