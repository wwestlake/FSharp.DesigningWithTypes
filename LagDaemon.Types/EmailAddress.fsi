namespace LagDaemon.Types

type EmailAddress


module EmailAddress =


    val create : s:string -> EmailAddress
    val validate : EmailAddress -> EmailAddress
    val createAndValidate : (string -> EmailAddress)
    val value : EmailAddress -> string
    val apply : f:(string -> 'a) -> EmailAddress -> 'a
    val map : rawfunc:(EmailAddress -> EmailAddress) ->
                invalidfunc:(EmailAddress -> EmailAddress) ->
                  unverifiedfunc:(EmailAddress -> EmailAddress) ->
                    verifiedfunc:(EmailAddress -> EmailAddress) ->
                      e:EmailAddress -> EmailAddress
    val id : x:EmailAddress -> EmailAddress


