

namespace LagDaemon.Types


type Identifier 


module Identifier =

    val create : unit -> Identifier
    val fromGuid : guid:System.Guid -> Identifier
    val value : id:Identifier -> System.Guid
    val toString : id:Identifier -> string
    val fromString : str:string -> Result<Identifier,string>


