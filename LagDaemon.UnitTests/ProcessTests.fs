module LagDaemon.ProcessTests

open System
open System.Text.RegularExpressions
open LagDaemon.Types
open NUnit.Framework
open FsCheck
open FsCheck.NUnit



[<SetUp>]
let Setup () =
    ()



[<Property>]
let ``Process creates a single action process`` a b =
    let proc = Process.create (fun a b -> b)
    printfn "proc %A %A = %A" a b b

    Process.run proc a b = b


