namespace LagDaemon.Types


[<AutoOpen>]
module Configuration =

    open FSharp.Configuration


    type ConfigFile = YamlConfig<"Config.yaml">

    let systemConfiguration = new ConfigFile()

   
   
