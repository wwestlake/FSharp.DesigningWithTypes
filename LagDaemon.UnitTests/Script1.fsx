

#r @"C:\Users\Bill.Westlake\.nuget\packages\fsharp.quotations.evaluator\1.1.3\lib\net45\FSharp.Quotations.Evaluator.dll"

open Microsoft.FSharp.Quotations

open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Quotations.Patterns
open FSharp.Quotations.Evaluator

<@@ 1 + 2 @@>



let print_string str = printfn "%s" str

open Microsoft.FSharp.Quotations
// A typed code quotation.
let expr : Expr<int> = <@ 1 + 1 @>
// An untyped code quotation.
let expr2 : Expr = <@@ 1 + 1 @@>

let expr3 : Expr<int> = <@ 
                        let a = 1 + 1 
                        let b = 4 * a
                        let c = b * b
                        b
                    @>


expr3 |> QuotationEvaluator.Evaluate

