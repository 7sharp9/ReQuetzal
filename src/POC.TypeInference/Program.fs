open System
open Ast2

[<EntryPoint>]
let main argv =
    try
        //inference algorith1
        // List.iter (Ast.tryExp Ast.myEnv) Ast.examples

        // //inference algorithm2
        // [Ast2.example1; Ast2.example2; Ast2.example3; Ast2.example4; Ast2.example5]
        // |> List.iter (fun e -> Ast2.resetId()
        //                        printfn "********************"
        //                        let ty = Ast2.infer Ast2.Env.empty 0 e
        //                        let generalizedTy = Ast2.generalize (-1) ty
        //                        printfn "%s" (Ast2.exp.toString e)
        //                        printfn ": %s" (Ast2.ty.toString generalizedTy)
        //                        printfn "" )

        //inference algorithm2 with row poly extension
        // printfn "--------------------------"
        // printfn "Row Polymorphism extension*"

        // [ 
        //   RowPoly.example1
        //   RowPoly.example2
        //   RowPoly.example3
        //   RowPoly.example4
        //   RowPoly.example5
        //   RowPoly.example6
        //   RowPoly.example7
        //   RowPoly.example8
        //   RowPoly.example9
        //   RowPoly.example10
        //   RowPoly.example11
        //   RowPoly.example12
        //   RowPoly.example13
        //   RowPoly.example14 ]
        // |> List.iter (fun e -> RowPoly.resetId()
        //                        printfn "********************"
        //                        printfn "%s" (RowPoly.exp.toString e)
        //                        let ty = RowPoly.infer RowPoly.basicEnv 0 e
        //                        let generalizedTy = RowPoly.generalize (-1) ty
        //                        printfn "********************"
        //                        printfn "%A" ty
        //                        printfn "********************"
        //                        printfn "%A" generalizedTy
        //                        printfn "********************"
        //                        printfn ": %s" (RowPoly.ty.toString generalizedTy)
        //                        printfn "" )
        let free = Ast3.test1 |> Ast3.Typ.freeTypeVariables
        printfn "free type variables for %A is:\n%A" Ast3.test1 free
    with
    | ex -> printfn "%s" ex.Message
    0

