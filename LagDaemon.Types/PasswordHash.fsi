namespace LagDaemon.Types

type PasswordHash



module PasswordHash =

    val value : pw:PasswordHash -> string
    
    val hash : password:string -> iterations:int -> PasswordHash
    val fastHash : password:string -> PasswordHash
    val strongHash : password:string -> PasswordHash
    val uberHash : password:string -> PasswordHash
    val verify : hashedPassword:PasswordHash -> password:string -> bool

