namespace LagDaemon.Types

module ZipCode =

    type T
    val createWithCont : success:(T -> 'a) -> failure:(string -> 'a) -> s:string -> 'a
    val create : s:string -> T option
    val apply : f:(string -> 'a) -> T -> 'a
    val value : e:T -> string


