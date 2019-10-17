namespace LagDaemon.Types

module Mail =

    open System
    open System.IO

    type SmtpMessage = {
        sender: string list
        replyTo: string
        recipients: string list
        cc_recipients: string list
        bcc_recipients: string list
        subject: string
        body: string
    }

    type SmtpClientFS () =
        let config = systemConfiguration
        let mutable (outStream: Stream) = null

        member this.Send() = ()

        member this.Out 
            with set(value) = outStream <- value


    and SmtpProtocolHandler() =
        let protocol = "EHLO"
        let EOL = [0x0D; 0x0A] // cr/lf
        member this.send() = ()

