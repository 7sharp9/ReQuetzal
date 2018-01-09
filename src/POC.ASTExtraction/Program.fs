// Learn more about F# at http://fsharp.org

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open System.Runtime.InteropServices.WindowsRuntime
open System.Xml.Linq

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
    | None -> failwith "Something went wrong during parsing.  No parseTree!"

let rec visitPattern (node: SynPat) =
  printf "Node: %s " (node.GetType().Name)
  match node with
  | SynPat.Wild(_range) -> 
      printfn "underscore pattern"
  | SynPat.Named(pat, name, _, _, _range) ->
      visitPattern pat
      printfn "named: '%s'" name.idText
  | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) ->
      let names = String.concat "." [ for i in ident -> i.idText ]
      printfn "identifier: %s" names
  | pat ->
      printfn "other pattern: %A" pat

let rec visitExpression (node: SynExpr) =
  printf "Node: %s " (node.GetType().Name)
  match node with
  | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
      // Visit all sub-expressions
      printfn "Conditional:"
      visitExpression cond
      visitExpression trueBranch
      falseBranchOpt |> Option.iter visitExpression 

  | SynExpr.LetOrUse(_, _, bindings, body, _) ->
      // Visit bindings (there may be multiple 
      // for 'let .. = .. and .. = .. in ...'
      printfn "LetOrUse with the following bindings:"
      for binding in bindings do
        let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, init, m, sp)) = binding
        visitPattern pat 
        visitExpression init
      // Visit the body expression
      printfn "And the following body:"
      visitExpression body
  | SynExpr.Const(cst, _range) ->
      printfn "Const: %A" cst
  | SynExpr.App(exprAtomicFlag, isInfix, funExpr, argExpr, _range) ->
        (visitExpression funExpr)
        (visitExpression argExpr)
     
  | SynExpr.Ident(ident) ->
      printfn "Ident: %A" ident
  | expr -> printfn "visitExpression - not supported expression: %A" expr

let visitDeclarations decls = 
  for declaration in decls do
    printf "Node: %s " (declaration.GetType().Name)
    match declaration with
    | SynModuleDecl.Let(isRec, bindings, range) ->
        // Let binding as a declaration is similar to let binding
        // as an expression (in visitExpression), but has no body
        for binding in bindings do
          let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) = binding
          visitPattern pat 
          visitExpression body         
    | _ -> printfn "visitDeclarations - not supported declaration: %A" declaration

let visitModulesAndNamespaces modulesOrNss =
  for moduleOrNs in modulesOrNss do
    let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, m)) = moduleOrNs
    if isMod then printfn "module: %A" lid
    else printfn "Namespace: %A" lid
    visitDeclarations decls

let niceParseTree =
   match parseTree with
   | ParsedInput.ImplFile(ParsedImplFileInput(filename, isScript, qualifierFilename, scopedPragmas, hashDirectives, modules, (isLastCompiland, isExe))) ->
       visitModulesAndNamespaces modules
   | ParsedInput.SigFile _ -> failwith "F# Interface file not supported."

let typedAst =
    checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously

printfn "Errors: %A" typedAst.Errors

[<EntryPoint>]
let main argv =
    printfn "\n%A" parseTree
    printfn "\n%A" typedAst.AssemblyContents.ImplementationFiles.Head.Declarations
    0 // return an integer exit code
