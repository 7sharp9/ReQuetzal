namespace Compiler

module AssemblyInfo =
    open System.Runtime.CompilerServices

    [<assembly: RuntimeCompatibility(WrapNonExceptionThrows = true)>]
    do ()

module Error =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Reflection
    open Patterns

    /// Not used for the moment
    type ErrorPriority =
        | FatalError
        | ErrorRecoverable 
        | Warning 

    type IFailure = 
        abstract member Description : string
        abstract member Priority : ErrorPriority     
    

    module Helper =
        let inline private extractUnionCaseInfo (expr : Expr<'T>) =
            let rec aux (expr : Expr) =
                match expr with
                | NewUnionCase (uci,_) -> uci
                | Lambda (_,expr) -> aux expr
                | Let (_,_,expr) -> aux expr
                | _ ->
                    failwithf "Unexpected expression : Expected a union expression but received : { %A }"  expr
            
            aux expr


 
        let inline (|IsFailureArgs|_|) (expr : Expr< ^args -> ^T>) (failure : ^Failure when ^Failure :> IFailure)  =
            // Check if the failure type is a Union
            if FSharpType.IsUnion (failure.GetType()) then
                let expectedType = typeof< ^T>
                printfn "Test : { %A }" (typeof< ^Failure>,FSharpType.IsUnion (typeof< ^Failure>)) 
                let failureType = FSharpType.GetUnionCases(failure.GetType(),true).[0].DeclaringType
                // Compare expr type and failure type : should be equal
                if expectedType = failureType then
                    // extract Union Case Info, if the expr is not a union, this will throw (bad implementation on the
                    // the user side)
                    let expectedUCI = extractUnionCaseInfo expr
                    let (failureUCI,args) = FSharpValue.GetUnionFields(failure,typeof< ^T>)
                    // both Union Case Info should be the same
                    if expectedUCI = failureUCI then
                        // return the args 
                        let args =
                            match args.Length with
                            | 0 -> box ()
                            | 1 -> args.[0]
                            | _ -> FSharpValue.MakeTuple(args,typeof< ^args>)
                        // This is safe to do, as we know the type of expr : Expr< ^args -> ^T>
                        Some (args :?> ^args)
                    else
                        None
                else
                    None
            else
                None

        let inline (|IsFailureNoArgs|_|) (expr : Expr< ^T>)  (failure : ^Failure when ^Failure :> IFailure) =
             (|IsFailureArgs|_|)  <@ fun () -> %expr @> failure        





// module test =
//     open Error
//     open Helper

//     type Test =
//         | Testing of int * float
//         | Toz
//         interface IFailure with
//             member this.Description =
//                 match this with
//                 | Toz -> "Toz"
//                 | Testing (tokenType , token) -> 
//                     (sprintf "Testing => { %A } <=> { %A }" tokenType token)
                        
//             member __.Priority = ErrorPriority.FatalError

//     let failure = Testing (5,2.0)
//     let e =
//         match failure with
//         | IsFailureArgs <@ Testing @> args -> printfn "%A" (failure,args)
//         | IsFailureNoArgs <@ Toz @> -> printfn "%A" failure
//         | _ -> failwith "failed"

//     // let e = extractUnionCaseInfo <@ Testing (5,2.0) @>

//     // let v = extractUnionCaseInfo <@ Toz @>
