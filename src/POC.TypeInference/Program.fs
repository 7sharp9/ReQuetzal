open System
open Ast

[<EntryPoint>]
let main argv =
    List.iter (tryExp myEnv) examples
    0

