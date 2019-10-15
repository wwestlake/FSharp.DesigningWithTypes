namespace LagDaemon.Types


[<AutoOpen>]
module ResultExtensions =

    open System
    
    let ofOption error = function Some s -> Ok s | None -> Error error
    
    
    let Return x = Ok x

    let ReturnFrom (m: Result<_, _>) = m

    let BindR f m =
        match m with
        | Ok value -> f value
        | Error msg -> Error msg


    let BindO m (error: (Option<'T> * 'E)) f = m |> ofOption error |> Result.bind f

    let Zero () = None

    let Combine m f = Result.bind f m

    let Delay (f: unit -> _) = f

    let Run f = f()

    let TryFinally m compensation =
          try ReturnFrom(m)
          finally compensation()

    let Using (res:#IDisposable) body =
          TryFinally (body res) (fun () -> match res with null -> () | disp -> disp.Dispose())

    let rec While guard (f: (unit -> 'a)) =
          if not (guard()) then Ok () else  
          do f() |> ignore 
          While guard f

    let For (sequence:seq<_>) body =
            Using 
                (sequence.GetEnumerator()) 
                (fun enum -> 
                    While (enum.MoveNext) (Delay (fun () -> body enum.Current)) )
    
    let ( >>= ) m f = BindR f m
    let ( >=> ) f1 f2 =
        f1 >> (BindR f2)

    type ResultBuilder() =
        member __.Return(x) = Ok x
    
        member __.ReturnFrom(m: Result<_, _>) = m
    
        member __.Bind(m, f) = Result.bind f m
        member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f
    
        member __.Zero() = None
    
        member __.Combine(m, f) = Result.bind f m
    
        member __.Delay(f: unit -> _) = f
    
        member __.Run(f) = f()
    
        member __.TryWith(m, h) =
            try __.ReturnFrom(m)
            with e -> h e
    
        member __.TryFinally(m, compensation) =
            try __.ReturnFrom(m)
            finally compensation()
    
        member __.Using(res:#IDisposable, body) =
            __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())
    
        member __.While(guard, f) =
            if not (guard()) then Ok () else
            do f() |> ignore
            __.While(guard, f)
    
        member __.For(sequence:seq<_>, body) =
            __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))
    
    
    let result = new ResultBuilder()
    
    [<AutoOpen>]
    module ResultHelpers =
        

        /// Convert a list of Result<'T,'F> to a (Result<'T list, 'F list>,Result<'T list, 'F list>) 
        /// returns a tuple of a (Ok list, Error list)
        let fromResultList list =            
            let rec loop rest okacc failacc =
                match rest with
                | [] -> 
                    match failacc with
                    | [] ->  Ok (List.rev okacc, List.rev failacc)
                    | _ -> Error (List.rev okacc, List.rev failacc)
                | head::tail -> 
                    match head with
                    | Ok h ->
                        loop tail (h :: okacc) failacc 
                    | Error msg -> loop tail okacc (msg :: failacc) 

            loop list [] [] 

        /// Executes a list of functions and returns the result of the composition.
        /// This function executes all functions in the list and returns the
        /// final result.  If any function in the list fails, the functon fails
        let executeList  list init =
            let rec inner rest f =
                match rest with
                | [] -> (f init)
                | head::[] -> (head >=> f) init
                | head1::head2::tail -> inner (tail) (head1 >=> head2)

            inner list (fun a -> Ok a)

        /// Executes all the functions in the list using the init value as
        /// the input to each function and returns a list of results 
        /// both Ok and Error may be in the list
        let executeListAll list init =
            let rec inner rest acc =
                match rest with
                | [] -> acc
                | head::tail -> inner tail ((head init) :: acc)
            inner list [] |> List.rev |> fromResultList


        let test list :Result<('a list * 'b list),('a list * 'b list)> =
            list |> fromResultList

        let test1  : Result<(int list * string list),(int list * string list)> =
            test [Ok 1; Ok 2; Error "bad"; Error "worse"; Ok 4; Ok 7; Error "Again?"]
                
        let test2  : Result<(int list * string list),(int list * string list)> =
            test [Ok 1; Ok 2; Ok 4; Ok 7]

        let test3  : Result<(int list * string list),(int list * string list)> =
            test [Error "bad"; Error "worse"; Error "Again?"]

        let test4 : Result<int,string>=
            let f1 a = Ok (a + 1)
            let f2 a = Ok (a + 2)
            let f3 a = Ok (a + 3)
            let f4 a = Error "Bad Info"
            let f5 a = Ok (a + 4)

            2 |> ([f1; f2; f3;f4;f5 ] |> executeList) 

        let test5 : Result<int,string> =
            let f1 a = Ok (a + 1)
            let f2 a = Ok (a + 2)
            let f3 a = Ok (a + 3)
            let f4 a = Ok (a + 4)

            2 |> ([f1; f2; f3; f4 ] |> executeList) 


        let test6 =
            let f1 a = Ok (a + 1)
            let f2 a = Ok (a + 2)
            let f3 a = Ok (a + 3)
            let f4 a = Error "Bad Info"
            let f5 a = Ok (a + 4)

            executeListAll [f1; f2; f3; f4; f5] 2 

