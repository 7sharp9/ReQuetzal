// Learn more about F# at http://fsharp.org

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

let checker = FSharpChecker.Create(keepAssemblyContents=true, ImplicitlyStartBackgroundWork=false)

let fileName = "/Users/davethomas/Documents/GitHub/ReQuetzal/src/POC.ASTExtraction/ReQuestsal_1_1.fs"
let projectFilename = "ReQuetsal.fsproj"

let assembly = Reflection.Assembly.GetCallingAssembly()
let referencedAssemblies = assembly.GetReferencedAssemblies()
let dependencies =
    [   yield sprintf "-r:%s" "/usr/local/share/dotnet/shared/Microsoft.NETCore.App/2.0.0/mscorlib.dll"
        yield sprintf "-r:%s" "/usr/local/share/dotnet/shared/Microsoft.NETCore.App/2.0.0/netstandard.dll"
        yield sprintf "-r:%s" "/usr/local/share/dotnet/shared/Microsoft.NETCore.App/2.0.0/System.Private.CoreLib.dll"
        for a in referencedAssemblies do
            let aa = System.Reflection.Assembly.Load(a.FullName)
            yield sprintf "-r:%s" aa.Location ]

printfn "%A" dependencies
let arguments =
    [|
        //yield "--simpleresolution"
        yield "--out:" + System.IO.Path.ChangeExtension(fileName, ".exe")
        yield "--platform:anycpu"
        yield "--fullpaths"
        yield "--flaterrors"
        yield "--target:exe"
        yield "--noframework"
        //yield sprintf "-r:%s" assembly.FullName
        yield "--targetprofile:netstandard"
        yield! dependencies
        yield fileName |]

printfn "arguments:\n%A" arguments
let projectOptions =
            
            checker.GetProjectOptionsFromCommandLineArgs(projectFilename, arguments)

let input = File.ReadAllText("/Users/davethomas/Documents/GitHub/ReQuetzal/src/POC.ASTExtraction/ReQuestsal_1_1.fs")

let projOptions = checker.GetProjectOptionsFromScript(fileName, input) |> Async.RunSynchronously

let parsingOptions, _errors = checker.GetParsingOptionsFromProjectOptions(projectOptions)

  // Run the first phase (untyped parsing) of the compiler
let parseFileResults = checker.ParseFile(fileName, input, parsingOptions) |> Async.RunSynchronously

let parseTree = 
    match parseFileResults.ParseTree with
    | Some tree -> tree
    | None -> failwith "Something went wrong during parsing!"

let typedAst =
    checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously

printfn "Errors: %A" typedAst.Errors

[<EntryPoint>]
let main argv =
    printfn "%A" parseTree
    printfn "%A" typedAst.AssemblyContents.ImplementationFiles.Head.Declarations
    0 // return an integer exit code
