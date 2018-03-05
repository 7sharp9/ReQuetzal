namespace Compiler
open System
open System.IO

module AST =
    open LexerTokenization
    open Parser
    open Parsing
//    let t = 5 
//     // *****************************************************************************************************
//     // AST => might need to change but these Unions represent the AST
//     // *****************************************************************************************************
//     type Name = Name of string
//     type VarName = VarName of string


//     type Literal =
//         | String of string
//         | Bool of bool
//         | Int of int
//         | Float of float 

//     type Expr =
//         // For example : x in `f(x)` => Var "x"
//         | Var of VarName
//         | Literal of Literal
//         | Let of Name * Expr * Expr
//         | Lambda of Name * Expr
//         | Application of Expr * Expr


//     // *****************************************************************************************************
//     // Type tree => to be used for type inference and type checking
//     // *****************************************************************************************************
//     // For example : `∀ α. α -> int -> α` => `α` is a type variable : TypeVariable "α" BUT `int` is not a type variable, it is a type constant
//     type TypeVariable = TypeVariable of string 
//     type TypeConstant = TypeConstant of string

//     type Type =
//         | TypeLambda of Type * Type
//         | TypeVariable of TypeVariable
//         | TypeConstant of TypeConstant

//     // Type Scheme represent polymorphic types : `id: ∀α. α -> α` for instance
//     // `id: ∀αβ. α -> β -> int -> list α * β ` is decomposed into
//     // `∀αβ` before the . which represents the bounded type variables `α and β` which are bounded to the quantifier `∀`
//     // The F# type system allow us to represent two part what is before the . which is a list of type variable and what is after which is the type itself
//     type Scheme = 
//         {   BoundedTypeVariables    : Set<TypeVariable>  // To ensure that each type variable is unique, we use a Set  
//             EnclosedType            : Type  }

//     // The Environment is the container of all information (everything, type scheme, bounded variables, variable names ...)
//     // Each variable found in the Expr tree has an associated type scheme which could be generalized (with some type holes to fill) 
//     // or instanciated (every hole is filled).
//     type Environment = Environment of Map<VarName,Scheme>

//     // Substitutions are just mapping from type variables to types that could be applied to a specific environment.
//     type Substitutions = Substitutions of Map<VarName,Type>

    type StreamWriter with
        member x.writeAsyncLine(input:string) = x.WriteLineAsync(input) |> Async.AwaitTask
        member x.writeAsyncNewLines(N:int) = 
            async{
                for _ in 1..N do
                    return! x.writeAsyncLine("")
            }


    [<EntryPoint>]
    let main argv =


        let inputsTYPE = 
            [
                "int"
                "int -> float"
                "(int -> (float -> int))"
                "(int -> (float -> int)) -> None"
                "(float -> int) -> None"
                "int -> (float -> int)) -> None"
                ""
                "int -> float ->"
            ]

        let inputsPARAMS = 
            [
                "f"
                "(f)"
                "(f :int)"
                "(f :int ,g : float,h)"
                "(f :int ,g,h )"
                "(f,g,h)"
                "(f :int ,g : float,h : (int -> float -> (float -> None) ) )"
                "(f,g,(h)"
                ""
            ]    


        let inputsEXPR = 
            [
                "let x = 5"
                "let x = 5 + 3"
                "let y = false"
                "let e = 5+3+4+(3+4*5)"
            ]    


        let parseTest (writer:StreamWriter) (parser:unit -> Parser<'T>) (input:string) =
            async{
                printfn "===================================" 
                printfn "TEST ==> { %s }" input 
                printfn "===================================" 
                let tokens = lexing input
                let parsed = runParser tokens (parser())

                match parsed with
                | Success (_,_,s,t) -> 
                    do! writer.writeAsyncLine(sprintf "SUCCESS ==> { %s }" input)
                    do! writer.writeAsyncLine(sprintf "%A" s)
                    do! writer.writeAsyncNewLines 2
                    do! writer.writeAsyncLine(sprintf "%A" t)
                    do! writer.writeAsyncNewLines 4
                | Failure failure -> 
                    do! writer.writeAsyncLine(sprintf "FAILURE => { %s }" input)
                    do! writer.writeAsyncLine(sprintf "%A" failure)
                    do! writer.writeAsyncNewLines 2
                    do! writer.writeAsyncLine(sprintf "%s" (failure.Description))
                    do! writer.writeAsyncNewLines 4
            }

        let parseTests (writer:StreamWriter) (name:string) (parser:unit -> Parser<'T>) (inputs:string list) =
            async{
                printfn "==========================================" 
                printfn "TEST SUITE ==> { %s }" name
                printfn "==========================================" 
                do! writer.writeAsyncLine(sprintf "==========================================" )
                do! writer.writeAsyncLine(sprintf "TEST SUITE ==> { %s }" name)
                do! writer.writeAsyncLine(sprintf "==========================================" )
                do! writer.writeAsyncNewLines 2

                for input in inputs do
                    return! parseTest writer parser input

                do! writer.writeAsyncNewLines 10
            }

        let run () =
            async{
                let path = @"C:\Users\Lleutch\Workspace\test.txt"
                use writer = new StreamWriter(File.Create(path))
    
                
                do! parseTests writer "PARAMETERS" parsePTParam inputsPARAMS
                do! parseTests writer "TYPE DEFINITION" parsePTTypeDef inputsTYPE
                do! parseTests writer "EXPRESSIONS" parsePTExpr inputsEXPR
            }

        run () |> Async.RunSynchronously
//        Async.Sleep 5000 |> Async.RunSynchronously
        0 // return an integer exit code
