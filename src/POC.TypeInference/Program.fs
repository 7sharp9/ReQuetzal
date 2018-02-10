open System
//open Ast2

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

        let mutableTests =
            [ RowPoly.example1
              RowPoly.example2
              RowPoly.example3
              RowPoly.example4
              RowPoly.example5
              RowPoly.example6
              RowPoly.example7
              RowPoly.example8
              RowPoly.example9
              RowPoly.example10
              RowPoly.example11 ]
            //   RowPoly.example12
            //   RowPoly.example13
            //   RowPoly.example14 ]

        let runMutableTestBank(showInference) =
            mutableTests
            |> List.iter (fun e -> RowPoly.resetId()
                                   
                                   let ty = RowPoly.infer RowPoly.basicEnv 0 e
                                   let generalizedTy = RowPoly.generalize (-1) ty
                                   if showInference then
                                       printfn "expression: %s" (RowPoly.exp.toString e)
                                       printfn "inferred: %s" (RowPoly.ty.toString generalizedTy))

        runMutableTestBank(true)

        let mutableTimes =
            [for i in 1..100 ->
                let sw = Diagnostics.Stopwatch.StartNew()
                runMutableTestBank(false)
                sw.Stop()
                sw.Elapsed.TotalMilliseconds ]

        printfn "Mutable Average bank = %f" (mutableTimes |> List.average)
        printfn "Mutable Average individual = %f" (mutableTimes |> List.averageBy (fun t -> t / 11.0))

        //---------------------------------------------------------------


        let tests =
            [   Ast3.b1; Ast3.b2; Ast3.b3; Ast3.b4; (*Ast3.b5;*) Ast3.b6; Ast3.b7
                Ast3.test1; Ast3.test2; Ast3.test3; Ast3.test4; Ast3.test5]

        let runTestBank(showInference) =
            tests
            |> List.iter (fun (name, exp) ->
                                   Ast3.resetId()
                                   
                                   let inferred = exp |> Ast3.typeInference Map.empty 
                                   if showInference then
                                       printfn "%s" name
                                       printfn "Expression: %A" exp
                                       printfn "inferred: %s\n\n" (Ast3.Typ.toString inferred))
        
        runTestBank(true)

        let times =
            [for i in 1..100 ->
                let sw = Diagnostics.Stopwatch.StartNew()
                runTestBank(false)
                sw.Stop()
                sw.Elapsed.TotalMilliseconds ]

        printfn "Average bank = %f" (times |> List.average)
        printfn "Average individual = %f" (times |> List.averageBy (fun t -> t / 11.0))


    with
    | ex -> printfn "%s" ex.Message
    0

