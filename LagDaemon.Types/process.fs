namespace LagDaemon.Types


type Process<'Tp,'Up> =
    | Start of Process<'Tp,'Up> list
    | End  of Operation<'Tp,'Up>
    | Action of Action<'Tp,'Up>
    | Decision of Decision<'Tp,'Up>


and Action<'Ta,'Ua> =
    | ImediateAction of Operation<'Ta,'Ua> * Process<'Ta,'Ua> list
    | DelayedAction of DelayedOperation<'Ta,'Ua> * Process<'Ta,'Ua> list

and Decision<'Td,'Ud> = 
    | Yes of Process<'Td,'Ud> list
    | No of Process<'Td,'Ud> list

and Operation<'To,'Uo> = Operation of ('To -> 'Uo -> 'Uo)
and DelayedOperation<'Tdo,'Udo> = DelayedOperation of (('Udo -> 'Udo) -> 'Tdo -> 'Udo -> 'Udo)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Process =
    open System

    let idop = Operation (fun _ u -> u)

    /// Creates a single imediate action process based on 'f'
    let create<'T,'U> f =
        Start [Action <| ImediateAction (Operation f, [End idop])]

    

    let exec (Operation op) t u = op t u
    let execDelayed (DelayedOperation op) f t =  op f t

    let rec run proc t u =
        let rec procRunner plist pt pu =
            match plist with
            | [] -> pu
            | head::tail -> procRunner tail pt (run head pt pu)

        match proc with
        | Start sproc -> procRunner sproc t u
        | End eop -> exec eop t u
        | Action action -> 
            match action with
            | ImediateAction (iop, iproc) -> let iu = (exec iop t u) in 
                                                        procRunner iproc t iu
            | DelayedAction (dop, dproc) -> let du = (execDelayed dop (procRunner dproc t) t u) in du
                                                         
        | Decision decision -> 
            match decision with
            | Yes yproc -> procRunner yproc t u
            | No nproc -> procRunner nproc t u


