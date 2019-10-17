namespace LagDaemon.Types

[<RequireQualifiedAccess>]
module TcpIp =

    open System.Net.Sockets
    open System.IO

    type Connection = {
        client: TcpClient
        stream: Stream
        reader: StreamReader 
        writer: StreamWriter 
    }

    let connect (address:string) port = 
        let result = async {
            let client = new TcpClient()
            try 
                do client.Connect(address, port)
                let stream = client.GetStream()
                return {
                    client = client
                    stream = stream
                    reader = new StreamReader(stream)
                    writer = new StreamWriter(stream)
                } |> Result.Ok

            with
            | excn -> return Result.Error excn
        } 
        result |> Async.RunSynchronously


