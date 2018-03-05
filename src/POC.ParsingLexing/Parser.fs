namespace Compiler
open LexerTypes
open LexerTokenization
open System
open System.Collections.Generic
open System.Reflection.Metadata
open System.Runtime.Versioning


// module AST =
    
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
//         | If of Expr * Expr * Expr
// open AST


// module ParserTypes =
//     open AST

//     let defaultIndent = 4
//     type IInvokeIndentRule =
//         abstract member Invoke : Location 

//     type IndentLevel = IndentLevel of int
        
//     type IndentRule = 
//         | IndentRule of (Location -> Location -> IndentLevel -> unit)
//         member x.Invoke l1 l2 indentLevel = 
//             let (IndentRule fn) = x 
//             fn l1 l2 indentLevel

//     let checkIRLevel = 
//         fun l1 l2 (IndentLevel indent) ->  
//             if sumColumn (defaultIndent * indent |> Column ) l1.Column = l2.Column then
//                 ()
//             else
//                 failwith "not properly indented"
//         |> IndentRule 


//     let sameLine l1 l2 = 
//         if l1.Line = l2.Line then
//             ()
//         else
//             failwith "not same line"
//     let lowerLine l1 l2 =  
//         if l1.Line < l2.Line then
//             ()
//         else
//             failwith "not same line"
//     let higherLine l1 l2 = 
//         if l1.Line > l2.Line then
//             ()
//         else
//             failwith "not same line"
    
//     let sameColumn l1 l2 = 
//         if l1.Column = l2.Column then
//             ()
//         else
//             failwith "not same line"
//     let lowerColumn l1 l2 =  
//         if l1.Column < l2.Column then
//             ()
//         else
//             failwith "not same line"
//     let higherColumn l1 l2 = 
//         if l1.Column > l2.Column then
//             ()
//         else
//             failwith "not same line"


//     type IfThenElse_IndentRule =
//         {   IfLocation          : Location
//             ThenLocation        : Location 
//             ElseLocation        : Location 
//             ExprIfLocation      : Location 
//             ExprElseLocation    : Location
//         }
//         member x.Invoke() =
//             do sameLine x.IfLocation x.ThenLocation
//             do checkIRLevel.Invoke x.IfLocation x.ElseLocation (IndentLevel 0)
//             do checkIRLevel.Invoke x.ExprIfLocation x.IfLocation (IndentLevel 1) 
//             do checkIRLevel.Invoke x.ExprIfLocation x.ExprElseLocation (IndentLevel 0)

//     let upToNextToken (condition: Token -> bool) (tokens: Token list) =
//         let rec aux (tokens:Token list) (tokensConsumed) =
//             match tokens with
//             | [] -> failwith "No Next Token"
//             | token::left ->
//                 if condition token then
//                     (List.rev tokensConsumed,token,left)
//                 else
//                     aux left (token::tokensConsumed)

//         aux tokens []


//     let nextKeywordTokenIs (keyword:Keyword) (tokens:Token list) =
//         let condition (token:Token) =
//             match token.TokenType with
//             | TokenType.Keyword _ -> true
//             | _ -> false

//         let (consumed,nextToken,left) = upToNextToken condition tokens
//         if Keyword keyword <> nextToken.TokenType then
//             failwith "Unexpected Keyword"  
       
//         (consumed,nextToken,left)

//     let nextTokenBlock (location:Location) (tokens:Token list) =
//         let condition (token:Token) = token.Location.Column <= location.Column
//         let (consumed,nextToken,left) = upToNextToken condition tokens
//         (consumed,nextToken::left)


//     let (|IfThenElse|_|) (tokens:Token list) =
//         let ifToken = tokens.Head
//         match ifToken.TokenType with
//         | TokenType.Keyword(Keyword.If) -> 
//             let (condition,thenToken,tokens) = nextKeywordTokenIs Keyword.Then tokens           
//             // do call for condition
//             let (expr1,elseToken,tokens) = nextKeywordTokenIs Keyword.Else tokens
//             let (expr2,tokens) = nextTokenBlock ifToken.Location tokens
//             // do call for expr1 and expr2
//             let (exprIfLocation:Location)   = getLocation expr1
//             let (ExprElseLocation:Location) = getLocation expr2
 
//             let rule =
//                 {   IfLocation          = ifToken.Location
//                     ThenLocation        = thenToken.Location
//                     ElseLocation        = elseToken.Location
//                     ExprIfLocation      = exprIfLocation
//                     ExprElseLocation    = ExprElseLocation
//                 }                
//             rule.Invoke()


//         None            



//     // let x =
//     //     Some Block
//     //            

//     let ( [|a|]) = [|10|]

//     type SubBlock = SubBlock of SubBlock list

//     type Block =
//         {
//             Expr : Expr
//             SubBlocks : Block
//         } 

    // *************************************
    // Steps                
    // *************************************
    //  1) Get token list
    //  2) Define set of rules (recursive) to generate each possible union case  
    //  2.1) Create all AP Tokenizer from matchToken function representing the lexical grammar of of each token (regex)
    //  2.2) Create a function which defines the overall lexical grammar, rule priority and longest match invariants
    //  2.3) Loop over that function and generate a token with current state of tokenization ...

// add: int -> int -> int
// add a b =
//     a + b


// quetzalProg := list of function

// function := signature? function_def
// signature := identifier COLON type_def

// identifier := IDENTIFIER 

// type_def := identifier EQUAL function_body

// function_def := identifier {parameter}+ EQUAL function_body
// function_body := expr

// expr :=   LITERAL 
//         | identifier
//         | LPar expr RPar 
//       //  | lookup expr    
//         | expr expr
//         | expr BinOp expr
//         | UnaryOp expr
//         | expr {COMMA expr}+ 
//         | expr {expr}+
//         | LET let_binding expr

// parameter := identifier {COMMA parameter}* 
//             | LPar parameter RPar 
//             | LPar parameter COLON type_def RPar

// let_binding := parameter = expr







    // type_def :=   LPar type_def RPar type_def_prime
    //             | type_value type_def_prime
    // type_def_prime :=  type_def ARROW type_def type_def_prime
    //                  | epsilon
    // type_value := identifier            

    // parameter := identifier {COMMA parameter}* 
    //             | LPar parameter RPar 
    //             | LPar parameter COLON type_def RPar

    // expr :=   LITERAL  expr_prime
    //         | identifier expr_prime
    //         | LPar expr RPar expr_prime
    //         | UnaryOp expr expr_prime
    //         | LET let_binding expr expr_prime

    // expr_prime: =  BinOp expr expr_prime
    //              | {COMMA expr}+ expr_prime
    //              | {expr}+ expr_prime
    //              | epsilon


    // let_binding := parameter = expr

    // function_body := expr

    // function_def := identifier {parameter}+ EQUAL function_body

    // signature := identifier COLON type_def

    // function := signature? function_def


    // quetzalProg := (function)* EOF



// module test =
//     open LexerTypes

// //     STRUCTURES     =>> ANALYZER COMPONENTS

// //     string          =>> LEXER 
// // =>> Token Stream    =>> PARSER
// // =>> Parse Tree      =>> CONVERTER 
// // =>> AST



//     // type_def :=   LPar type_def RPar type_def_prime
//     //             | type_value type_def_prime
//     // type_value := identifier            
//     type PTTypeDef =
//         | PTTypeDefBlock of PTTypeDef * PTTypeDefPrime 
//         | PTTypeValue of Ident * PTTypeDefPrime

//     // type_def_prime :=  ARROW type_def type_def_prime
//     //                  | epsilon
//     and PTTypeDefPrime =
//         | PTArrow of PTTypeDef * PTTypeDefPrime
//         | PTEpsilon

//     // parameter := identifier {COMMA parameter}* 
//     //             | LPar parameter RPar 
//     //             | LPar parameter COLON type_def RPar
//     type PTParam = 
//         | PTMultParam of Ident * PTParam list
//         | PTParamBlock of PTParam 
//         | PTParamTyped of PTParam * PTTypeDef

//     // expr :=   LITERAL  expr_prime
//     //         | identifier expr_prime
//     //         | LPar expr RPar expr_prime
//     //         | UnaryOp expr expr_prime
//     //         | LET let_binding expr expr_prime
//     type PTExpr =
//         | PTLit of Literal * PTExprPrime
//         | PTIdent of Ident * PTExprPrime
//         | PTBlock of PTExpr * PTExprPrime
//         | PTUnaryOp of Operator * PTExpr * PTExprPrime
//         | PTLet of PTLetBinding * PTExpr * PTExprPrime
    
//     // expr_prime: =  BinOp expr expr_prime
//     //              | {COMMA expr}+ expr_prime
//     //              | {expr}+ expr_prime
//     //              | epsilon
//     and PTExprPrime =
//         | PTBinaryOp of Operator * PTExpr * PTExprPrime
//         | PTTuple of PTExpr list * PTExprPrime
//         | PTAppli of PTExpr list * PTExprPrime
//         | PTEpsilon

//     // let_binding := parameter = expr
//     and PTLetBinding = PTLetBinding of PTParam * PTExpr        

//     // function_body := expr
//     type PTFunctionBody = PTExpr

//     // function_def := identifier {parameter}+ EQUAL function_body
//     type PTFunctionDef = 
//         PTFunctionDef of Ident * PTParam list * PTFunctionBody

//     // signature := identifier COLON type_def
//     type PTSignature = PTSignature of Ident * PTTypeDef

//     // function := signature? function_def
//     type PTFunction = PTFunction of PTSignature option * PTFunctionDef

//     // quetzalProg := (function)* EOF
//     type PTQuetzalProg = PTQuetzalProg of PTFunction list
     
//     let rec parsePTTypeDef (tokens : Token list) =
//         let paramToken,tokens = tokens.Head,tokens.Tail
//         match paramToken.TokenType with
//         | Separator LParentheses ->
//             let (ptTypeDef1,tokens):PTTypeDef * Token list = parsePTTypeDef tokens
//             let paramToken,tokens = tokens.Head,tokens.Tail
//             match paramToken.TokenType with
//             | Separator RParentheses ->
//                 let ptTypeDef2,tokens = parsePTTypeDefPrime tokens
//                 PTTypeDefBlock (ptTypeDef1,ptTypeDef2),tokens
//             | _ -> failwith "unexpected token"
//         | Identifier ident -> 
//             let ptTypeDef,tokens = parsePTTypeDefPrime tokens
//             PTTypeValue (ident,ptTypeDef),tokens
//         | _ -> failwith "unexpected token"        

//     and parsePTTypeDefPrime (tokens : Token list) =
//         try
//             let paramToken,tokens = tokens.Head,tokens.Tail
//             match paramToken.TokenType with
//             | Separator Arrow ->
//                 let ptTypeDef2,tokens = parsePTTypeDef tokens
//                 let ptTypeDefPrime,tokens = parsePTTypeDefPrime tokens
//                 PTArrow (ptTypeDef2,ptTypeDefPrime),tokens
//             | _ -> failwith "unexpected stuff"

//         with
//         | _ -> PTTypeDefPrime.PTEpsilon,tokens



//     let rec parsePTParam (tokens : Token list) = 
//         let paramToken,tokens = tokens.Head,tokens.Tail
//         match paramToken.TokenType with
//         | Identifier ident -> 
//             let rec auxParams (tokens : Token list) (parameters : PTParam list) =
//                 try
//                     let ptParam,tokens = parsePTParam tokens
//                     auxParams tokens (ptParam::parameters)
//                 with
//                 | _ ->
//                      PTMultParam (ident,List.rev parameters),tokens 

//             auxParams tokens []
//         | Separator LParentheses ->
//             let ptParam,tokens = parsePTParam tokens
//             let paramToken,tokens = tokens.Head,tokens.Tail
//             match paramToken.TokenType with
//             | Separator RParenthese -> 
//                 PTParamBlock ptParam , tokens            
//             | Separator Colon ->
//                 let ptTypeDef, tokens = parsePTTypeDef tokens
//                 let paramToken,tokens = tokens.Head,tokens.Tail
//                 match paramToken.TokenType with
//                 | Separator RParenthese -> 
//                     PTParamTyped (ptParam,ptTypeDef) , tokens            
//                 | _ -> failwith "unexpected token"
//             | _ -> failwith "unexpected token"
//         | _ -> failwith "unexpected token"
        

//     let rec parsePTExpr (tokens : Token list) =
//         let exprToken,tokens = tokens.Head,tokens.Tail
//         match exprToken.TokenType with
//         | Literal literal -> 
//             let ptExprPrime, tokens = parsePTExprPrime tokens
//             PTLit (literal , ptExprPrime) , tokens
//         | Identifier ident -> 
//             let ptExprPrime, tokens = parsePTExprPrime tokens
//             PTIdent (ident , ptExprPrime) , tokens
//         | Separator LParentheses -> 
//             let (ptExpr,tokens):PTExpr * Token list = parsePTExpr tokens
//             let rPar,tokens = tokens.Head,tokens.Tail
//             match rPar.TokenType with
//             | Separator RParentheses -> 
//                 let ptExprPrime, tokens = parsePTExprPrime tokens
//                 PTBlock (ptExpr , ptExprPrime) , tokens
//             | _  -> failwith "unexpected token, expected a ) separator"
//         | Operator Operator.Subs -> 
//             let ptExpr,tokens = parsePTExpr tokens
//             let ptExprPrime,tokens = parsePTExprPrime tokens
//             PTUnaryOp (Operator.Subs , ptExpr , ptExprPrime) , tokens
//         | Keyword Let ->
//             let ptLetBinding,tokens = parsePTLetBinding tokens
//             let ptExpr,tokens = parsePTExpr tokens
//             let ptExprPrime,tokens = parsePTExprPrime tokens
//             PTLet (ptLetBinding , ptExpr, ptExprPrime) , tokens
//         | _ -> failwith "unexpected token"

    
//     and parsePTExprPrime (tokens : Token list) = 
//         let (|ParsePTBinaryOp|_|) (tokens : Token list) =
//             match tokens with
//             | [] -> None 
//             | [token] when token.TokenType = EOF ->  Some (PTEpsilon,tokens) 
//             | token :: tokens -> 
//                 match token.TokenType with
//                 | Operator operator ->
//                     let ptExpr,tokens = parsePTExpr tokens
//                     let ptExprPrime,tokens = parsePTExprPrime tokens
//                     (PTBinaryOp (operator,ptExpr,ptExprPrime) , tokens)
//                     |> Some 
//                 | _ -> None

//         let (|ParsePTTuple|_|) (tokens : Token list) =
//             let rec aux (tokens : Token list) (tuple : PTExpr list) =
//                 match tokens with
//                 | [] -> None
//                 | [token] when token.TokenType = EOF -> Some (PTEpsilon,tokens)            
//                 | token :: tokens -> 
//                     match token.TokenType with
//                     | Separator Comma ->
//                         let ptExpr,tokens = parsePTExpr tokens
//                         aux tokens (ptExpr::tuple)
//                     | _ -> 
//                         let ptExprPrime,tokens = parsePTExprPrime tokens
//                         (PTTuple (List.rev tuple,ptExprPrime) , tokens)
//                         |> Some 
//             aux tokens []

//         let (|ParsePTAppli|_|) (tokens : Token list) =
//             let rec aux (tokens : Token list) (exprs : PTExpr list) =
//                 try
//                     let ptExpr,left = parsePTExpr tokens
//                     aux left (ptExpr::exprs)
//                 with
//                 | _ ->
//                     let ptExprPrime,tokens = parsePTExprPrime tokens
//                     (PTAppli (List.rev exprs,ptExprPrime) , tokens)
//                     |> Some 
//             aux tokens []

//         match tokens with
//         | ParsePTBinaryOp (ptExprPrime,tokens) 
//         | ParsePTTuple (ptExprPrime,tokens) 
//         | ParsePTAppli (ptExprPrime,tokens) -> ptExprPrime , tokens
//         | _ ->
//             match tokens with
//             | [token] when token.TokenType = EOF -> PTEpsilon , tokens.Tail
//             | _ -> failwith "unexpected token"


//     and parsePTLetBinding (tokens : Token list) =
//         let ptParam,tokens = parsePTParam tokens
//         let exprEqual,tokens = tokens.Head,tokens.Tail
//         match exprEqual.TokenType with
//         | Operator Equal -> 
//             let ptExpr,tokens = parsePTExpr tokens
//             (PTLetBinding (ptParam,ptExpr),tokens)
//         | _ -> failwith "unexpected token"


//     let parsePTFunctionBody = parsePTExpr

//     let parsePTParams (tokens:Token list) =
//         let rec aux (tokens:Token list) (iCan:bool) (ptParams:PTParam list) =
//             if iCan then
//                 let iCan,ptParams,rest =
//                     try
//                         let ptParam,rest = parsePTParam tokens
//                         true,ptParam::ptParams,rest
//                     with 
//                     | _ -> 
//                         false,ptParams,tokens
//                 aux rest iCan ptParams
//             else
//                 (List.rev ptParams,tokens)
//         aux tokens true []

//     let parsePTFunctionDef (tokens : Token list) =
//         let identifier,tokens = tokens.Head,tokens.Tail
//         match identifier.TokenType with
//         | Identifier ident -> 
//             let ptParams,tokens = parsePTParams tokens 
//             let equalToken,tokens = tokens.Head,tokens.Tail
//             match equalToken.TokenType with
//             | Operator Equal -> 
//                 let ptFunctionBody,tokens = parsePTFunctionBody tokens
//                 PTFunctionDef (ident, ptParams, ptFunctionBody) , tokens
//             | _ -> failwith "unexpected token, expected an = operator "
//         | _ -> failwith "unexpected token, expected an identifier"


//     let parsePTSignature (tokens : Token list) =
//         let identifier,tokens = tokens.Head,tokens.Tail
//         match identifier.TokenType with
//         | Identifier ident -> 
//             let colon,tokens = tokens.Head,tokens.Tail
//             match colon.TokenType with
//             | Separator Colon -> 
//                 let ptTypeDef,tokens = parsePTTypeDef tokens 
//                 Some (PTSignature (ident, ptTypeDef)) , tokens
//             | _ -> None , tokens
//         | _ -> failwith "unexpected token, expected an identifier"

//     let parsePTFunction (tokens : Token list) =
//         // let rec aux (tokens : Token list)  
//         let ptSignature,tokens = parsePTSignature tokens
//         let ptFunctionDef,tokens = parsePTFunctionDef tokens
//         PTFunction (ptSignature,ptFunctionDef) , tokens


//     let parsePTFunctions (tokens:Token list) =
//         let rec aux (tokens:Token list) (iCan:bool) (ptFunctions:PTFunction list) =
//             if iCan then
//                 let iCan,ptFunctions,rest =
//                     try
//                         let ptFunction,rest = parsePTFunction tokens
//                         true,ptFunction::ptFunctions,rest
//                     with 
//                     | _ -> 
//                         false,ptFunctions,tokens
//                 aux rest iCan ptFunctions
//             else
//                 (List.rev ptFunctions,tokens)
//         aux tokens true []

//     let parsePTQuetzalProg (tokens:Token list) =
//         let ptFunctions,rest = parsePTFunctions(tokens)
//         match rest with
//         | [token] when token.TokenType = EOF -> PTQuetzalProg ptFunctions
//         | _ -> failwith "parse error, expected EOF"


//     [<EntryPoint>]
//     let main argv =
        
//         let input = 
//             "add: int -> int -> int
// add a b =
//     a + b
    
// t x =
//     let y = x + 5
//     y + 6"



//         let res = 
//             lexing
//             >> parsePTQuetzalProg
        
//         printfn "START"

//         printfn "%A" (res input)

//         Async.Sleep 50000 |> Async.RunSynchronously

//         0

    

    // type Bin =
    //     | Plus
    //     | Less
    //     | Times
    //     | Div
    //     | Pow 

    // type Un =
    //     | Minus

    // type P =
    //     | Lit of int
    //     | Par of Arithmetic
    //     | Un of Un * P

    // and Arithmetic = 
    //     | Arithm of P * (Bin * P) list





    // let t = """
    // 5
    // 5 + 3
    // -5 + -3
    // 5 + 3 * 2 + 5
    // 5 + 3 / 2 + 5
    // 5 ^ 3 * 2 + 5
    // (5 + 3) * 2 + 5
    // 5 ^ (3 * 2) + 5    
    // """

    // let binPrec =
    //     [   "+" , 1
    //         "-" , 1                
    //         "*" , 2                
    //         "/" , 2                
    //         "^" , 3                
    //     ] |> Map.ofList     








// [<AutoOpen>]
// module Result =

//     type IFailure =
//         abstract member Reason : unit -> string
//         abstract member GetStackTrace : unit -> string
//         abstract member GetStackTraceUpToLevel : int -> string 

//     [<NoComparison>]
//     [<NoEquality>]
//     type Result<'a> =
//         | Success of 'a
//         | Failure of IFailure

//     [<NoComparison>]
//     [<NoEquality>]
//     type Delayed<'a> = Delayed of (unit -> Result<'a>)
  
//     type ResultBuilder() =
//         member __.Bind(Delayed m : Delayed<'a>, f : 'a -> Result<'b>) : Result<'b> =
//             match m() with
//             |Success elem -> f elem
//             |Failure s -> Failure s
                
//         member __.Return (x:'a) : Result<'a> = Success x
//         member __.ReturnFrom (Delayed m:Delayed<'a>) : Result<'a> = m()
//         member __.Zero () = Success ()
                
//         member __.Combine (a:Result<'a>,b:unit -> Result<'a>) : Result<'a> = 
//             let runnedB = b()
//             match a,runnedB with
//             | Success _ , Success b1 -> Success b1
//             | Success _ , Failure b1 -> Failure b1
//             | Failure a1, Success _  -> Failure a1
//             | Failure a1, Failure _  -> Failure a1

//         member __.Delay(f:unit -> Result<'a>) : (unit -> Result<'a>) = f
//         member __.Run(f:unit -> Result<'a>) = Delayed f
            
//     let result = ResultBuilder()

//     let runResult (Delayed res) = res()



// module State =

//     type IInternal = interface end

//     type State<'t, 'state> = 
//         | State of ('state -> 't * 'state)


//     type Delayed<'t,'state> = Delayed of (unit -> State<'t,'state>)

//     let bind (f:'t -> State<'u,'state>) (Delayed m:Delayed<'t,'state>) : State<'u,'state> = 
//         fun s -> 
//             let (State stateFunction) = m()
//             let (a, s') = stateFunction s 
//             let (State stateFunction') = (f a) 
//             stateFunction' s'
//         |> State


//     type StateBuilder() =
//         member __.Bind(m:Delayed<'t,'state>, k:'t -> State<'t,'state>) : State<'t,'state> = bind k m

//         member __.Return(a:'t) : State<'t,'state> = State (fun s -> (a,s))
//         member __.ReturnFrom(Delayed m:Delayed<'t,'state>) : State<'t,'state> = m()
//         member this.Zero() = this.Return ()

//         member this.Combine(s1 : State<'t,'state>, f2 : unit -> State<'t,'state>) : State<'t,'state> =  
//             this.Bind(Delayed (fun () -> s1), fun _ -> f2())

//         member __.Delay(f: unit -> State<'t,'state>) = f
//         member __.Run(f: unit -> State<'t,'state>) = Delayed f



//     // type StateBuilder< ^I when ^I :> IInternal>() =
//     //     member inline __.Bind(m:Delayed<'t,'state>, k:'t -> State<'t,'state>) : State<'t,'state> = bind k m

//     //     member __.Return(a:'t) : State<'t,'state> = State (fun s -> (a,s))
//     //     member __.ReturnFrom(Delayed m:Delayed<'t,'state>) : State<'t,'state> = m()
//     //     member this.Zero() = this.Return ()

//     //     member this.Combine(s1 : State<'t,'state>, f2 : unit -> State<'t,'state>) : State<'t,'state> =  
//     //         this.Bind(Delayed (fun () -> s1), fun _ -> f2())

//     //     member __.Delay(f: unit -> State<'t,'state>) = f
//     //     member __.Run(f: unit -> State<'t,'state>) = Delayed f

//     // let state = new StateBuilder()
    
//     let getState = 
//         Delayed (fun () -> (State (fun s -> (s,s))))
//     let putState s = 
//         Delayed (fun () -> (State (fun _ -> ((),s))))

//     let eval (Delayed m) state = 
//         let (State d) = m()
//         d state |> fst
//     let exec (Delayed m) state = 
//         let (State d) = m()
//         d state |> snd

//     let empty = fun s -> ((), s)
    


//     let state = new StateBuilder()


//     let t = 
//         state{
//             do! putState 3.2
//             let st = getState
//             return 5
//         }

//     let r = eval t 5.0
//     let s = exec t 2.





























// module IMonad =
//     type IInternal<'a> = interface end

//     type IDelayed<'a> = interface end

//     type IMonadBuilder =
//         abstract member Bind< 'a, 't when 't :> IDelayed<'a> >  : 't * ('a -> 't) -> 't
//             // IDelayed<'a> * ('a -> IDelayed<'b>) -> IDelayed<'b>
//         abstract member Return : 'a -> IDelayed<'a>
//         abstract member ReturnFrom : IDelayed<'a> -> IInternal<'a>
//         abstract member Zero : unit -> IInternal<unit>
//         abstract member Combine : IInternal<'a> * (unit -> IInternal<'a>) -> IInternal<'a>
//         abstract member Delay : (unit -> IInternal<'a>) -> (unit -> IInternal<'a>)
//         abstract member Combine : IInternal<'a> * (unit -> IInternal<'a>) -> IInternal<'a>
//         abstract member Run : (unit -> IInternal<'a>) -> IDelayed<'a>




// module Result =
//     open IMonad

//     type IFailure =
//         abstract member Reason : unit -> string
//         abstract member GetStackTrace : unit -> string
//         abstract member GetStackTraceUpToLevel : int -> string 

//     [<NoComparison>]
//     [<NoEquality>]
//     type Result<'a> =
//         | Success of 'a
//         | Failure of IFailure
//         interface IInternal<'a>

//     [<NoComparison>]
//     [<NoEquality>]
//     type Delayed<'a> = 
//         | Delayed of (unit -> Result<'a>)
//         interface IDelayed<'a>
//         // static member inline Unit<'a>() = () 

//     type ResultBuilder() =
//         interface IMonadBuilder with

//             member __.Bind(Delayed m:Delayed<'a>, f) =
//                 match m() with
//                 |Success elem -> f elem
//                 |Failure s -> Failure s
                   
//             member __.Return x = Success x
//             member __.ReturnFrom (Delayed m) = m()
//             member __.Zero () =  Success ()
                    
//             member __.Combine (a,b)= 
//                 let runnedB = b()
//                 match a,runnedB with
//                 | Success _ , Success b1 -> Success b1
//                 | Success _ , Failure b1 -> Failure b1
//                 | Failure a1, Success _  -> Failure a1
//                 | Failure _ , Failure b1 -> Failure b1

//             member __.Delay(f) = f
//             member __.Run(funToRun) =
//                 Delayed funToRun
            
//     let result = ResultBuilder()

//     let runResult (Delayed result) = result()


// module State =
//     open Result
    

//     type State<'T, 'State> = State of ('State -> IMonad<'T * 'State>)
//     type Delayed<'s, 'd> = Delayed of (unit -> State<'s, 'd>)


//     type StateBuilder<'State>() =
//         let bind ( k:'T -> State<'U,'State> ) (Delayed delayed:Delayed<'T,'State>) = 
//             fun s -> 
//                 let (State m) = delayed()
//                 match m s with
//                 | Success (a, s') ->
//                     // (a, s')
//                     let (State state) = (k a)
//                     (state s')
//                 | Failure failure -> Failure failure
//             |> State

//         member __.Bind(m:Delayed<'T,'State>, k:'T -> State<'U,'State>) : State<'U,'State> = bind k m
//         member __.Return(a) : State<'T,'State> = State(fun s -> Success(a,s))
//         member __.ReturnFrom(Delayed m:Delayed<'T,'State>) = m()
//         member this.Zero() = this.Return ()
//         member __.Combine(state1 : State<'a,'State>, delayedState2 : unit -> State<'a,'State>) = 
//             let state2 = delayedState2()
//             bind (fun _ -> state2) (Delayed (fun _ -> state1))
//         member __.Delay(f) = f
//         member __.Run(delayed) = Delayed delayed

//     let getState = 
//         let state = State(fun s -> Success(s,s)) 
//         (fun () -> state) |> Delayed
    
//     let putState s = 
//         let state = State(fun _ -> ((),s))
//         (fun () -> state) |> Delayed

//     let runData (Delayed delayed) state =
//         let (State m) = delayed() 
//         m state |> fst

//     let runState (Delayed delayed) state =
//         let (State m) = delayed() 
//         m state |> snd
//     let empty = fun s -> ((), s)



//     let parse = StateBuilder<Token List>()

module ParserErrors =


    type ParserFailure =
        // basic parsing failure
        | ExpectedIdentifier            of Token
        | ExpectedLiteral               of Token
        | ExpectedBinaryOp              of Token
        | ExpectedKeyword               of Keyword * Token
        | ExpectedSeparator             of Separator * Token
        | ExpectedOperator              of Operator * Token
        | CannotConsumeEmptyState 
        // wrapped failures with context
        | ParsingPTTypeDefFailure       of ParserFailure
        | ParsingPTTypeDefPrimeFailure  of ParserFailure
        | ParsingPTParamFailure         of ParserFailure
        | ParsingPTExprFailure          of ParserFailure
        | ParsingPTExprPrimeFailure     of ParserFailure
        member this.Description =
            let identation = "  "
            let (tokenError,location) =

                let appendIndentedLine description level toAppend =
                    description + "\n" + String.replicate level identation + toAppend               

                let rec getDescription (level:int) (description:string) (failure:ParserFailure) =
                    match failure with

                    | ExpectedIdentifier token ->
                        let description = 
                            appendIndentedLine description level
                                (sprintf "Expected an Identifier but received the Token : { %A }" token)
                        (description, Some token.Location)
                    | ExpectedLiteral token ->
                        let description = 
                            appendIndentedLine description level
                                (sprintf "Expected a Literal but received the Token : { %A }" token )
                        (description, Some token.Location)
                    | ExpectedBinaryOp token ->
                        let description = 
                            appendIndentedLine description level
                                (sprintf "Expected a Binary Operator but received the Token : { %A }" token)
                        (description, Some token.Location)
                    | CannotConsumeEmptyState ->
                        let description = 
                            appendIndentedLine description level
                                (sprintf "Cannot consume a token as the state is empty")
                        (description, None)
                    | ExpectedKeyword   (keyword    , token) ->
                        let description = 
                            appendIndentedLine description level
                                (sprintf "Expected a { Keyword %A } but received the Token : { %A }" keyword (token.TokenType))
                        (description, Some token.Location)
                    | ExpectedSeparator (separator  , token) ->
                        let description = 
                            appendIndentedLine description level
                                (sprintf "Expected a { Separator %A } but received the Token : { %A }" separator (token.TokenType) )
                        (description, Some token.Location)
                    | ExpectedOperator  (operator   , token) ->
                        let description = 
                            appendIndentedLine description level
                                (sprintf "Expected an { Operator %A } but received the Token : { %A }" operator (token.TokenType) )
                        (description, Some token.Location)


                    | ParsingPTTypeDefFailure       parserFailure -> 
                        ("Failed to Parse type definition"      ,parserFailure)
                        |> fun (description,parserFailure) -> getDescription (level+1) description parserFailure
                    | ParsingPTTypeDefPrimeFailure  parserFailure -> 
                        ("Failed to Parse type definition Prime",parserFailure)
                        |> fun (description,parserFailure) -> getDescription (level+1) description parserFailure
                    | ParsingPTParamFailure         parserFailure -> 
                        ("Failed to Parse parameters"           ,parserFailure)
                        |> fun (description,parserFailure) -> getDescription (level+1) description parserFailure
                    | ParsingPTExprFailure          parserFailure -> 
                        ("Failed to Parse Expression"           ,parserFailure)
                        |> fun (description,parserFailure) -> getDescription (level+1) description parserFailure
                    | ParsingPTExprPrimeFailure     parserFailure -> 
                        ("Failed to Parse Expression Prime"     ,parserFailure)
                        |> fun (description,parserFailure) -> getDescription (level+1) description parserFailure
                                    
                getDescription 0 "" this 

            
            match location with
            | None -> sprintf "Parsing Error : \n%s" tokenError 
            | Some location ->
                let (Line line) = location.Line
                let (Column column) = location.Column
                sprintf "Parsing Error : Line %i Column %i \n%s" line column tokenError



module Parser =
    open ParserErrors

    type Parser<'T> = RWSResult<unit,Token list,Token list,'T,ParserFailure>
    let parser = RWSResultBuilder<Token list>(Monoid.list)

    let getState : Parser<Token list>= rwsGetState Monoid.list
    let putState state : Parser<unit>= rwsPutState Monoid.list state
    let read : Parser<unit> = rwsRead Monoid.list
    let write value : Parser<unit> = rwsWrite value
    let runParser state (computation : Parser<'T>) : Result<unit,Token list,Token list,'T,ParserFailure>= rwsRun (state,()) computation

    let error (failure : ParserFailure) : Parser<'T> = RWSResult (fun() -> RWSRInternal (fun _ -> Failure failure))

    let catchError (handler : ParserFailure -> Parser<'T>) (computation : Parser<'T>) : Parser<'T> =
        parser{
            let! state = getState
            let result = runParser state computation
            match result with
            | Success _ -> return! computation
            | Failure failure -> return! handler failure
        }

    let parserNop: Parser<unit> = parser {return ()}

    let fromList (computation: Parser<'T> list) : Parser<'T list>=
        let rec aux (list:Parser<'T> list) output =
            parser{
                match list with
                | [] -> return List.rev output
                | hd::tl -> 
                    let! data = hd
                    return! aux tl (data::output)

            }
        aux computation []

    
    let tryWhileSeparator (failure:Separator) (action:Parser<'T>) : Parser<'T list> =
        parser{
            let mutable acc = []
            let mutable guard = true
            while guard do
                try
                    let! result = action
                    acc <- result::acc
                with
                | ExpectedSeparator(f,_) when f = failure -> 
                    guard <- false
                    return ()
            
            return List.rev acc
        }

    let private getParserFailureArgs<'a> (withSubArg:bool) (failure : ParserFailure) : Parser<'a> =
        parser{
            try
                let arguments =
                    let rec getArgs (withSubArg:bool) (parserFailure:ParserFailure) =
                        match parserFailure with
                        | ExpectedIdentifier token                  -> box token
                        | ExpectedLiteral token                     -> box token
                        | ExpectedBinaryOp token                    -> box token
                        | CannotConsumeEmptyState                   -> box ()
                        | ExpectedKeyword   (keyword    , token)    -> box (keyword    , token)
                        | ExpectedSeparator (separator  , token)    -> box (separator  , token)
                        | ExpectedOperator  (operator   , token)    -> box (operator   , token)

                        | ParsingPTTypeDefFailure       parserFailure  
                        | ParsingPTTypeDefPrimeFailure  parserFailure  
                        | ParsingPTParamFailure         parserFailure  
                        | ParsingPTExprFailure          parserFailure  
                        | ParsingPTExprPrimeFailure     parserFailure ->
                            if withSubArg then
                                getArgs withSubArg parserFailure
                            else
                                box parserFailure
                    getArgs withSubArg failure |> unbox<'a>
                return arguments
            with
            | _ ->
                return! error failure
        }

    let t = fun token -> ExpectedSeparator(Comma,token) |> ParsingPTExprFailure 


    let private parseUntilWithSubArg 
                    (failureCase: 'args -> ParserFailure) 
                    (withSubArg:bool) 
                    (isSubArg:'args -> bool) 
                    (computation:Parser<'T>) : Parser<'T list> =
        let rec aux count acc =
            parser{
                try 
                    let! result = computation
                    return! aux (count + 1) (result::acc) 
                with
                | catchedFailure ->
                    let! args = getParserFailureArgs<'args> withSubArg catchedFailure
                    if failureCase args = catchedFailure && isSubArg args && count > 0 then
                        return acc
                    else
                        return! error catchedFailure                        
            }
        
        aux 0 []

    let parseUntilWithArg (failureCase: 'args -> ParserFailure) (isSubArg:'args -> bool) (computation:Parser<'T>) : Parser<'T list> =
        parseUntilWithSubArg failureCase true isSubArg computation



    let parseUntil (failureCase: 'args -> ParserFailure) (computation:Parser<'T>) : Parser<'T list> =
        parseUntilWithSubArg failureCase false (fun _ -> true) computation
      

    let wrapFailure (failureCase : ParserFailure -> ParserFailure) (computation:Parser<'T> ) :Parser<'T>  =
        parser{
            try
                return! computation
            with
            | failure -> return! error (failureCase failure)
        }

// An exception of type 'System.InvalidCastException' occurred in Compiler.dll 
// but was not handled in user code: 'Unable to cast object of 
// type 'System.Tuple`2[Compiler.LexerTypes+Keyword,Compiler.LexerTypes+Token]' to type 'ParserFailure'.'

module ParserHelper =
    open Parser
    open ParserErrors

    let consumeToken : Parser<Token> =
        parser{
            let! tokens = getState 
            match tokens with
            | [] -> return! Parser.error CannotConsumeEmptyState
            | token::tokens ->
                do! putState tokens 
                return token
        }
    let restoreTokens (tokens : Token list) : Parser<unit> =
        parser{
            let! stateTokens = getState
            do! putState (tokens @ stateTokens)
        }

    let restoreToken (token : Token) : Parser<unit> =
        parser{
            let! tokens = getState
            do! putState (token::tokens)
        }
        

    let parseIdentifier : Parser<Ident> =
        parser{
            let! token = consumeToken
            match token.TokenType with
            | Identifier ident -> return ident 
            | _ -> return! Parser.error (ExpectedIdentifier token)
        }

    let private parseTokenType 
                    (tokenType      : 'a -> TokenType) 
                    (failure        : ('a * Token) -> ParserFailure) 
                    (tokenParameter : 'a) 
                    : Parser<unit> =
        parser{
            let! token = consumeToken
            if token.TokenType = tokenType tokenParameter then 
                return  ()
            else 
                return! Parser.error (failure (tokenParameter , token))
        }


    let parseOperator   = parseTokenType Operator ExpectedOperator
    let parseSeparator  = parseTokenType Separator ExpectedSeparator
    let parseKeyword    = parseTokenType Keyword ExpectedKeyword   

    // TODO : CHANGE TO HANDLE ONLY UNARY OPERATORS
    let parseUnaryOperator   = parseOperator
    let parseBinaryOp : Parser<Operator> = 
        parser{
            let! token = consumeToken
            match token.TokenType with
            | Operator op -> return op 
            | _ -> return! Parser.error (ExpectedBinaryOp token)
        }
    

    let parseLiteral : Parser<Literal> =
        parser{
            let! token = consumeToken
            match token.TokenType with
            | Literal lit -> return lit 
            | _ -> return! Parser.error (ExpectedLiteral token)
        }



module Parsing =
    open Parser
    open ParserErrors
    open ParserHelper
    open Error
    open Error.Helper



//     STRUCTURES     =>> ANALYZER COMPONENTS

//     string          =>> LEXER 
// =>> Token Stream    =>> PARSER
// =>> Parse Tree      =>> CONVERTER 
// =>> AST



    // // type_def :=   LPar type_def RPar type_def_prime
    // //             | type_value type_def_prime
    // // type_value := identifier            
    // type PTTypeDef =
    //     | PTTypeDefBlock of PTTypeDef * PTTypeDefPrime 
    //     | PTTypeValue of Ident * PTTypeDefPrime

    // // type_def_prime :=  ARROW type_def type_def_prime
    // //                  | epsilon
    // and PTTypeDefPrime =
    //     | PTArrow of PTTypeDef * PTTypeDefPrime
    //     | PTEpsilon

    // // parameter := identifier {COMMA parameter}* 
    // //             | LPar parameter RPar 
    // //             | LPar parameter COLON type_def RPar
    // type PTParam = 
    //     | PTMultParam of Ident * PTParam list
    //     | PTParamBlock of PTParam 
    //     | PTParamTyped of PTParam * PTTypeDef

    // // expr :=   LITERAL  expr_prime
    // //         | identifier expr_prime
    // //         | LPar expr RPar expr_prime
    // //         | UnaryOp expr expr_prime
    // //         | LET let_binding expr expr_prime
    // type PTExpr =
    //     | PTLit of Literal * PTExprPrime
    //     | PTIdent of Ident * PTExprPrime
    //     | PTBlock of PTExpr * PTExprPrime
    //     | PTUnaryOp of Operator * PTExpr * PTExprPrime
    //     | PTLet of PTLetBinding * PTExpr * PTExprPrime
    
    // // expr_prime: =  BinOp expr expr_prime
    // //              | {COMMA expr}+ expr_prime
    // //              | {expr}+ expr_prime
    // //              | epsilon
    // and PTExprPrime =
    //     | PTBinaryOp of Operator * PTExpr * PTExprPrime
    //     | PTTuple of PTExpr list * PTExprPrime
    //     | PTAppli of PTExpr list * PTExprPrime
    //     | PTEpsilon

    // // let_binding := parameter = expr
    // and PTLetBinding = PTLetBinding of PTParam * PTExpr        

    // // function_body := expr
    // type PTFunctionBody = PTExpr

//     // function_def := identifier {parameter}+ EQUAL function_body
//     type PTFunctionDef = 
//         | PTFunctionDef of Ident * PTParam list * PTFunctionBody

//     // signature := identifier COLON type_def
//     type PTSignature = 
// //        | PTSignature of Ident * PTTypeDef
//         | PTSignature of Ident // TO REMOVE
//         | SigNot of Ident // TO REMOVE
//     // function := signature? function_def
//     type PTFunction = PTFunction of PTSignature option * PTFunctionDef

//     // quetzalProg := (function)* EOF
//     type PTQuetzalProg = PTQuetzalProg of PTFunction list


    // let parsePTSignature =
    //     parser{
    //         let! ident = parseIdentifier
    //         try
    //             do! parseSeparator Colon 
    //             return PTSignature ident
    //         with
    //         | ExpectedSeparator (Colon,_) -> return (SigNot ident)         
    //     }

    // let parsePTFunctionDef (tokens:Token list) =
    //     let (ident,tokens) = parseIdentifier tokens
    //     let tokens = parseOperator Equal tokens
    //     (FnNot ident, tokens)

    // let parseFunction (tokens : Token list) =
    //     let (ptSignature,tokens) = parsePTSignature tokens
    //     let (ptFunctionDef,tokens) = parsePTFunctionDef tokens
    //     (PTFunction (ptSignature,ptFunctionDef) , tokens)



    // let parsePTQuetzalProg (tokens : Token list) =
    //     let rec auxParser (tokens : Token list) (ptFunctions: PTFunction list) =
    //         match tokens with
    //         | [token] when token.TokenType = EOF -> PTQuetzalProg (ptFunctions |> List.rev)
    //         | [] -> failwith "No Possible" 
    //         | _ -> 
    //             let (ptFunction,tokensLeft) = parseFunction tokens
    //             auxParser tokensLeft (ptFunction::ptFunctions)

    //     auxParser tokens []



    // type_def :=   LPar type_def RPar type_def_prime
    //             | type_value type_def_prime
    // type_value := identifier            
    type PTTypeDef =
        | PTTypeDefBlock of PTTypeDef * PTTypeDefPrime 
        | PTTypeValue of Ident * PTTypeDefPrime

    // type_def_prime :=  ARROW type_def type_def_prime
    //                  | epsilon
    and PTTypeDefPrime =
        | PTArrow of PTTypeDef * PTTypeDefPrime
        | PTEpsilon



    let rec parsePTTypeDef () =
        parser{
            printfn "parsePTTypeDef"
            try
                let! ident = parseIdentifier
                let! ptTypeDefPrime = parsePTTypeDefPrime ()
                return PTTypeValue (ident,ptTypeDefPrime)
            with
            | ExpectedIdentifier _ ->
                do! parseSeparator LParentheses            
                let! ptTypeDef = parsePTTypeDef()
                do! parseSeparator RParentheses                
                let! ptTypeDefPrime = parsePTTypeDefPrime ()
                return PTTypeDefBlock (ptTypeDef,ptTypeDefPrime) 
        }
        |> wrapFailure ParsingPTTypeDefFailure

    and parsePTTypeDefPrime () =
        parser{
            printfn "parsePTTypeDefPrime"
            try
                do! parseSeparator Arrow
                let! ptTypeDef = parsePTTypeDef()
                let! ptTypeDefPrime = parsePTTypeDefPrime ()
                return PTArrow (ptTypeDef,ptTypeDefPrime)             
            with
            | ExpectedSeparator (Arrow,_) -> 
                return PTEpsilon
        }
        |> wrapFailure ParsingPTTypeDefPrimeFailure




    // parameter := 
    //         | LPar parameter RPar
    //         | identifier COMMA parameter
    //         | identifier COLON type_def {COMMA parameter}?
    //         | identifier

    type PTParam = 
        | PTIdent of Ident
        | PTParamBlock of PTParam 
        | PTIdentSeparated of Ident * PTParam        
        | PTParamTyped of Ident * PTTypeDef * PTParam option

    let rec parsePTParam () =
        parser{
            try
                printfn "parsePTParam"
                do! parseSeparator LParentheses
                let! ptParam = parsePTParam()
                do! parseSeparator RParentheses
                return PTParamBlock ptParam
            with
            | ExpectedSeparator (LParentheses,_) ->
            try
                let! ident = parseIdentifier
                do! parseSeparator Comma
                let! ptParam = parsePTParam()
                return PTIdentSeparated (ident,ptParam)
            with
            | ExpectedSeparator (Comma,_) ->
            try
                let! ident = parseIdentifier
                do! parseSeparator Colon
                let! ptTypeDef = parsePTTypeDef ()
                try
                    do! parseSeparator Comma
                    let! ptParam = parsePTParam()                        
                    return PTParamTyped (ident,ptTypeDef,Some ptParam)     
                with
                | ExpectedSeparator (Comma,_) ->
                    return PTParamTyped (ident,ptTypeDef,None)                                 
            with
            | ExpectedSeparator (Colon,_) ->
                let! ident = parseIdentifier
                return PTIdent ident
        }
        |> wrapFailure ParsingPTParamFailure

    // expr := | LPar expr RPar expr_prime
    //         | LITERAL  expr_prime
    //         | identifier expr_prime
    //         | UnaryOp expr expr_prime
    //         | LET let_binding expr_prime
    type PTExpr =
        | PTBlock of PTExpr * PTExprPrime
        | PTLit of Literal * PTExprPrime
        | PTIdent of Ident * PTExprPrime
        | PTUnaryOp of Operator * PTExpr * PTExprPrime
        | PTLet of PTLetBinding * PTExprPrime
    
    // expr_prime: =  BinOp expr expr_prime
    //              | {expr}+ expr_prime
    //              | {COMMA expr}+ expr_prime
    //              | epsilon
    and PTExprPrime =
        | PTBinaryOp of Operator * PTExpr * PTExprPrime
        | PTAppli of PTExpr list * PTExprPrime
        | PTTuple of PTExpr list * PTExprPrime
        | PTEpsilon
    
    // let_binding := parameter = expr
    and PTLetBinding = PTLetBinding of PTParam * PTExpr        

    let rec parsePTExpr () =
        parser{
            try
                printfn "parsePTExpr"
                do! parseSeparator LParentheses
                let! ptExpr = parsePTExpr()
                do! parseSeparator RParentheses
                let! ptExprPrime = parsePTExprPrime ()
                return PTBlock (ptExpr,ptExprPrime)
            with
            | ExpectedSeparator (LParentheses,_) ->
            try
                let! literal = parseLiteral
                let! ptExprPrime = parsePTExprPrime ()
                return PTLit (literal,ptExprPrime)
            with
            | ExpectedLiteral _ ->
            try
                let! ident = parseIdentifier
                let! ptExprPrime = parsePTExprPrime ()
                return PTIdent (ident,ptExprPrime)
            with
            | ExpectedIdentifier _ ->
            try 
                do! parseUnaryOperator Subs
                let! ptExpr = parsePTExpr()
                let! ptExprPrime = parsePTExprPrime ()
                return PTUnaryOp (Subs,ptExpr,ptExprPrime)                          
            with
            | ExpectedOperator (Subs,_) ->
                do! parseKeyword Let
                let! ptLetBinding = parsePTLetBinding ()
                let! ptExprPrime = parsePTExprPrime ()
                return PTLet (ptLetBinding,ptExprPrime)                           
        }
        |> wrapFailure ParsingPTExprFailure

    and parsePTExprPrime () =
        parser{
            try
                printfn "parsePTExprPrime"
                let! binaryOp = parseBinaryOp
                printfn "ici"
                let! ptExpr = parsePTExpr()
                printfn "la"
                let! ptExprPrime = parsePTExprPrime()
                printfn "fin"
                return PTBinaryOp(binaryOp,ptExpr,ptExprPrime)
            with
            | ExpectedBinaryOp _ ->
            try 
                let! ptExprs =
                    parser{
                        printfn "expr list"
                        return! parsePTExpr ()
                    } 
                    |> parseUntil ParsingPTExprFailure

                printfn "exprPrimesss"
                let! ptExprPrime = parsePTExprPrime()                    
                printfn "end 2"
                return PTAppli (ptExprs,ptExprPrime)
            with
            | f ->
            let! s = getState
            printfn "----"
            printfn "[%A]" s
            printfn "----"
            try 
                let! ptTuple = 
                    parser{
                        do! parseSeparator Comma
                        return! parsePTExpr ()
                    } 
                    |> parseUntilWithArg
                        (ExpectedSeparator >> ParsingPTExprFailure) 
                        (fun (op,_) -> op = Comma)
    
                let! ptExprPrime = parsePTExprPrime()
                return PTTuple (ptTuple,ptExprPrime)
            with
            | f ->
                printfn "----"
                printfn "[%A]" f
                printfn "----"
                return PTEpsilon
        }
        |> wrapFailure ParsingPTExprPrimeFailure

    and parsePTLetBinding() =
        parser{
            printfn "parsePTLetBinding"
            let! ptParam = parsePTParam ()
            do! parseOperator Equal
            let! ptExpr = parsePTExpr ()
            return PTLetBinding (ptParam,ptExpr)
        }

    // let rec parsePTParam () =
    //     parser{
    //         try
    //             do! parseSeparator LParentheses
    //             let! pTParam = parsePTParam()
    //             do! parseSeparator RParentheses
    //             return PTParamBlock pTParam

    //         with
    //         | ExpectedSeparator (LParentheses,_) ->
    //             try
    //                 let! pTParam = parsePTParam()
    //                 do! parseSeparator Colon
    //                 let! ptTypeDef = parsePTTypeDef ()
    //                 return PTParamTyped (pTParam,ptTypeDef)
    //             with
    //             | ExpectedSeparator (Colon,_) ->
    //                 let! ident = parseIdentifier
    //                 let! paramList =
    //                     Parser.tryWhileSeparator Comma
    //                         (parser{
    //                             do! parseSeparator Comma
    //                             return! parsePTParam()
    //                         })

    //                 return PTMultParam (ident,paramList)            
    //     }


    // let input = 
    //     "ot >"
    // let tokens = lexing input


    // let parsed = runParser tokens parsePTSignature
    // match parsed with
    // | Success (_,_,s,_) -> printfn " ==> { %A }" s
    // | Failure failure -> ()

    // let parsePTSignature =
    //     parser{
    //         let! ident = parseIdentifier
    //         do! parseSeparator Colon 
    //             |> catchError( fun failure -> parser{
    //                     match failure with
    //                     | ExpectedTokenType _ ->
    //                         do! restoreToken
    //                         return (SigNot ident)
    //                     | _ -> 
    //                         return! Parser.error failure
    //                 }                     
    //                )   
    //         return PTSignature ident
         
    //     }


    // let parsePTSignature =
    //     parser{
    //         let! ident = parseIdentifier
            
    //         return!
    //             parser{
    //                 do! parseSeparator Colon
    //                 return PTSignature ident
    //             }
    //             |> ParseFailure.OnFailure ( 
    //                 <@ ExpectedTokenType @> ,
    //                 fun ( _ , consumedToken) -> parser{
    //                     do! restoreToken consumedToken
    //                     return (SigNot ident)
    //                 }                     
    //                )            
    //     }


    // let parsePTSignature =
    //     parser{
    //         let! ident = parseIdentifier            
    //         return! parseSeparator Colon
    //             |> ParseFailure.OnFailure(

    //                 <@ ExpectedTokenType @>, 

    //                 (fun ( _ , consumedToken) -> parser{
    //                     do! restoreToken consumedToken
    //                     return (SigNot ident)                        
    //                 }), 

    //                 parser{return PTSignature ident}
    //              )
            
  
    //     }


    // let parsePTFunctionDef (tokens:Token list) =
    //     let (ident,tokens) = parseIdentifier tokens
    //     let tokens = parseOperator Equal tokens
    //     (FnNot ident, tokens)

    // let parseFunction (tokens : Token list) =
    //     let (ptSignature,tokens) = parsePTSignature tokens
    //     let (ptFunctionDef,tokens) = parsePTFunctionDef tokens
    //     (PTFunction (ptSignature,ptFunctionDef) , tokens)



    // let parsePTQuetzalProg (tokens : Token list) =
    //     let rec auxParser (tokens : Token list) (ptFunctions: PTFunction list) =
    //         match tokens with
    //         | [token] when token.TokenType = EOF -> PTQuetzalProg (ptFunctions |> List.rev)
    //         | [] -> failwith "No Possible" 
    //         | _ -> 
    //             let (ptFunction,tokensLeft) = parseFunction tokens
    //             auxParser tokensLeft (ptFunction::ptFunctions)

    //     auxParser tokens []



    // // type_def :=   LPar type_def RPar type_def_prime
    // //             | type_value type_def_prime
    // // type_value := identifier            
    // type PTTypeDef =
    //     | PTTypeDefBlock of PTTypeDef * PTTypeDefPrime 
    //     | PTTypeValue of Ident * PTTypeDefPrime

    // // type_def_prime :=  ARROW type_def type_def_prime
    // //                  | epsilon
    // and PTTypeDefPrime =
    //     | PTArrow of PTTypeDef * PTTypeDefPrime
    //     | PTEpsilon



    // let rec parsePTTypeDef (tokens : Token List) =
    //     let parsePTTypeDefBlock (tokens : Token List) =
    //         let tokens = parseSeparator LParentheses tokens
    //         let ptTypeDef,tokens = parsePTTypeDef tokens
    //         let tokens = parseSeparator RParentheses tokens
    //         let ptTypeDefPrime,tokens = parsePTTypeDefPrime tokens
    //         PTTypeDefBlock (ptTypeDef,ptTypeDefPrime),tokens

    //     let parsePTTypeValue (tokens : Token List) =
    //         let ident,tokens = parseIdentifier tokens
    //         let ptTypeDefPrime,tokens = parsePTTypeDefPrime tokens
    //         PTTypeValue (ident,ptTypeDefPrime) , tokens

    //     try
    //         parsePTTypeDefBlock tokens
    //     with
    //     | _ -> parsePTTypeValue tokens
        

    // and parsePTTypeDefPrime (tokens : Token List) =
    //     let parsePTArrow (tokens : Token List) =
    //         let tokens = parseSeparator Arrow tokens
    //         let ptTypeDef,tokens = parsePTTypeDef tokens
    //         let ptTypeDefPrime,tokens = parsePTTypeDefPrime tokens
    //         PTArrow (ptTypeDef,ptTypeDefPrime) , tokens
        
    //     try 
    //         parsePTArrow tokens
    //     with
    //     | _ -> PTEpsilon , tokens    




    // let input = 
    //     "ot -> int -> float -> (int -> float) -> Test"


//     let input = 
//         "add:
// add =

// Toz   =

// Toz   =
// Toz   =
// Test:
// Test = "


    // let res = 
    //     lexing
    //     >> parsePTTypeDef
    
    // printfn "START"

    // printfn "%A" (res input)




// module TokenParser =

//     type IError =
//         abstract member Description : string

//     type ParsingOutcome<'TOutcome> =
//         | Result of 'TOutcome
//         | Error of IError
    
//     type IParseable =
//         abstract member 


//     type TokenParser<'TOutcome> = TokenParser of (Token list -> Token list * ParsingOutcome<'TOutcome> ) 

//     let runParser (TokenParser parser) = parser 

//     // Parser a single token
//     let pToken (tokenToMatch:Token) =
//         let innerParser (tokens:Token list) =
//             match tokens with
//             | [] -> (tokens,Error "It's done!!") // TODO : Add a case that's it's done
//             | token::left ->
//                 if token = tokenToMatch then
//                     (left,Result token)
//                 else
//                     (tokens,Error (sprintf "Expected %A but got %A" tokenToMatch token) )
                    
//         TokenParser innerParser

//     let pAnd parser1 parser2 =

//         let innerParser (tokens:Token list) =
//             let (tokens,result1) = runParser parser1 tokens
//             match result1 with
//             | Result outcome1 -> 
//                 let (tokens,result2) = runParser parser2 tokens
//                 match result2 with
//                 | Result outcome2 -> (tokens, Result (outcome1,outcome2))
//                 | Error err -> (tokens)
//             match tokens with
//             | [] -> (tokens,Error "It's done!!") // TODO : Add a case that's it's done
//             | token::tokens ->
//                 if token = tokenToMatch then
//                     (tokens,Result token)
//                 else
//                     (tokens,Error (sprintf "Expected %A but got %A" tokenToMatch token) )
                    
//         TokenParser innerParser
        


//     let parseExpr, parseExprRef : PTExprParser * PTExprParser ref = createParserForwardedToRef()
//     let parseTypeDef (tokens : Token list) =
//         let parseTypeDef, parseTypeDefRef : Parser<PTTypeDef, unit> * Parser<PTTypeDef, unit> ref = 
//             createParserForwardedToRef()
        
//         let parsePTArrow () =
//             parseTypeDef .>> 


//         let (|IsTypeValue|_|) (token:Token) =
//             match token.TokenType with
//             | Identifier ident -> 

//         let rec aux tokens acc =
//             match tokens with
//             | [] -> acc
//             | token::tokens ->
                


//     let parseExpr (tokens : Token list) =
//         let rec aux tokens acc =
//             match tokens with
//             | [] -> acc
//             | token::tokens ->





// v    quetzalProg := list of function

// v    function := signature? function_def
// v    signature := identifier COLON type_def

// v?    identifier := IDENTIFIER 

// v    type_def :=   type_def ARROW type_def
//                 | LPar type_def RPar

// v    function_def := identifier {parameter}+ EQUAL function_body
// v    function_body := expr

// v    expr :=   LITERAL 
//             | identifier
//             | LPar expr RPar 
//             | expr BinOp expr
//             | UnaryOp expr
//             | expr {COMMA expr}+ 
//             | expr {expr}+
//             | LET let_binding expr

// v    parameter := identifier {COMMA parameter}* 
//                 | LPar parameter RPar 
//                 | LPar parameter COLON type_def RPar

// v    let_binding := parameter = expr



















