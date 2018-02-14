namespace Compiler
open System

module AST =
   let t = 5 
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


    [<EntryPoint>]
    let main argv =
        printfn "Hello World from F#!"
        0 // return an integer exit code
