open System

[<EntryPoint>]
let main argv =
    try
        //inference algorith1
        List.iter (Ast.tryExp Ast.myEnv) Ast.examples

        //inference algorithm2
        [Ast2.example1; Ast2.example2; Ast2.example3; Ast2.example4; Ast2.example5]
        |> List.iter (fun e -> Ast2.resetId()
                               printfn "********************"
                               let ty = Ast2.infer Ast2.Env.empty 0 e
                               match ty with
                                   | Ast2.TConst  name -> printfn "tconst %s" name
                                   | Ast2.TApp(ty, tylist) -> printfn "tapp"
                                   | Ast2.TArrow(tylist, ty) -> printfn "tarrow: params: %A, ty: %A" tylist ty
                                   | Ast2.TVar(tvarref) -> printfn "tvar"

                               let generalizedTy = Ast2.generalize (-1) ty
                               match generalizedTy with
                                   | Ast2.TConst  name -> printfn "tconst %s" name
                                   | Ast2.TApp(ty, tylist) -> printfn "tapp"
                                   | Ast2.TArrow(tylist, ty) -> printfn "tarrow: params: %A, ty: %A" tylist ty
                                   | Ast2.TVar(tvarref) -> printfn "tvar: %A" tvarref
                               printfn "%s" (Ast2.exp.toString e)
                               printfn ": %s" (Ast2.ty.toString generalizedTy)
                               printfn "" )

        //inference algorithm2 with row poly extension
        printfn "--------------------------"
        printfn "Row Polymorphism extension*"
        printfn "%s" (RowPoly.example13 |> RowPoly.exp.toString)

        [RowPoly.example6; RowPoly.example7; RowPoly.example8; RowPoly.example9; RowPoly.example10; RowPoly.example11; RowPoly.example12; RowPoly.example13]
        |> List.iter (fun e -> RowPoly.resetId()
                               printfn "********************"
                               let ty = RowPoly.infer RowPoly.basicEnv 0 e
                               let generalizedTy = RowPoly.generalize (-1) ty
                               let rec printExpr ty= 
                                   match ty with
                                   | RowPoly.TConst  name -> sprintf "tconst %s" name
                                   | RowPoly.TApp(ty, tylist) -> sprintf "tapp: %s [%s]" (printExpr ty) ( tylist |> List.map printExpr |> String.concat ", ") 
                                   | RowPoly.TArrow(tylist, ty) -> sprintf "tarrow: params: %A, ty: %A" tylist ty
                                   | RowPoly.TVar(tvarref) -> sprintf "tvar: %A" tvarref
                                   | RowPoly.TRowEmpty -> sprintf "{ }"
                                   | RowPoly.TRowExtend(name, ty, row) -> sprintf "record extend: name: %s type: %s record: %s" name (printExpr ty) (printExpr row)
                                   | RowPoly.TRecord(row) -> sprintf "record: %s" (printExpr row)
                               printfn "§:%s" (printExpr generalizedTy)
                               printfn "%s" (RowPoly.exp.toString e)
                               printfn ": %s" (RowPoly.ty.toString generalizedTy)
                               printfn "" )
    with
    | ex -> printfn "%s" ex.Message
    0

