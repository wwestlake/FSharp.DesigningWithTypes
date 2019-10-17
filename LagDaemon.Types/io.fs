namespace LagDaemon.Types


type IO<'T> = private | Action of (unit -> 'T)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module IO =
    let private raw  (Action f) = f
    let internal run  io         = raw io ()
    let private eff  g   io     = raw io () |> g
    let private bind io  rest   = Action (fun () -> io |> eff rest |> run)
    let private comb io1 io2    = Action (fun () -> run io1; run io2)
    let private rtrn x          = Action (fun () -> x)


    type IOBuilder() =
        member b.Return(x)              = rtrn x
        member b.ReturnFrom(io) : IO<_> = io
        member b.Delay(g) : IO<_>       = g ()
        member b.Bind(io, rest)         = bind io rest
        member b.Combine(io1, io2)      = comb io1 io2
        member b.Run(io)                = run io
    


[<AutoOpen>]
module PreludeIO =

    let io = new IO.IOBuilder()
    let (|Action|) io = IO.run io


    let putChar  (c:char)   = Action (fun () -> stdout.Write(c))
    let putStr   (s:string) = Action (fun () -> stdout.Write(s))
    let putStrLn (s:string) = Action (fun () -> stdout.WriteLine(s))
    let print x             = Action (fun () -> printfn "%A" x)
    let getChar     = Action (fun () -> stdin.Read() |> char |> string)
    let getLine     = Action (fun () -> stdin.ReadLine())
    let getContents = Action (fun () -> stdin.ReadToEnd())