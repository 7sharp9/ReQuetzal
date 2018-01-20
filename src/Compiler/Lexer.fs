namespace Compiler

module LexerTypes =

    // *************************************
    // Steps                
    // *************************************
    // v 1) Get lines from string file
    // v 2) Tokenize each line 
    // v 2.1) Create all AP Tokenizer from matchToken function representing the lexical grammar of of each token (regex)
    // v 2.2) Create a function which defines the overall lexical grammar, rule priority and longest match invariants
    // v 2.3) Loop over that function and generate a token with current state of tokenization ...

    type Ident = Ident of string

    type Keyword =
        | Let | Var
        | If | Then | Else
        | For | In | Do
        | Type
        | With

    type Separator =
        | LParentheses | RParentheses
        | LBracket | RBracket
        | LBraces | RBraces 
        | Comma | Colon

    type Operator =
        | Add | Mult | Div | Subs | And | Or
        | Inferior | Superior | Equal 
        | Assign 
       
    type Literal =
        | Unit
        | Bool of bool
        | Int of int | Float of float 
        | String of string

    type Comment =
        | LineComment of string
        | BlockComment of string 

    /// Represent the different possible tokens generated from the lexing
    type TokenType =
        | Identifier of Ident
        | Keyword of Keyword
        | Separator of Separator
        | Operator of Operator
        | Literal of Literal
        | Comment of Comment

    /// Represent the starting location of a particular element
    type Column = Column of int
    type Line   = Line of int
    type Location =
        {
            Line    : Line
            Column  : Column
        }

    /// Represents a single Token, we need to know it's name and value (Token Type) and the associated location 
    type Token =
        {
            TokenType   : TokenType
            Location    : Location
        }




module LexerMappings =
    open LexerTypes
    
    let mapOps =
        [   "<-"  , Assign
            "+"   , Add
            "*"   , Mult
            "/"   , Div
            "-"   , Subs
            "&&"  , And
            "||"  , Or
            "<"   , Inferior
            ">"   , Superior
            "="   , Equal 
        ] 
        |> Map.ofList


    let mapSeps =
        [   "(" , LParentheses
            ")" , RParentheses
            "[" , LBracket
            "]" , RBracket
            "{" , LBraces
            "}" , RBraces
            "," , Comma
            ":" , Colon
        ]
        |> Map.ofList

    let mapKeywords =
        [   "let"   , Let
            "var"   , Var
            "if"    , If
            "then"  , Then
            "else"  , Else
            "for"   , For
            "in"    , In
            "do"    , Do
            "Type"  , Type
            "With"  , With
        ]
        |> Map.ofList

        


module LexerRegexDefinition =

    // Basic Regex
    let char        = "[a-zA-Z]"
    let digit       = "[0-9]"


    // Literals
    let int         = sprintf "-?%s+" digit
    let float       = sprintf "-?%s+\.%s+" digit digit
    let unit        = sprintf "\([ ]*\)"
    let string      = sprintf "\".*\""
    let bool        = sprintf "true|false"

    // Comments
    let lineComment = sprintf "//[^\n\r]*"
    let blockComment= sprintf "\(\*[^\*\)]*\*\)"


    // WhiteSpaces
    let whitespace  = " "

    // End Of Files 
    let eof = "\z"

    // NewLines
    let newline     = "\n\r|\n|\r"

    // Identifiers or keywords
    let identifierOrKeyword  = sprintf "%s[a-zA-Z0-9]*" char

    // Operators
    let operators   = ["<\-";"\+";"\*";"/";"\-";"&&";"\|\|";"<";">";"="] |> String.concat("|")

    // Separators
    let separators  = ["\(";"\)";"\[";"\]";"\{";"\}";",";":"] |> String.concat("|")



module LexerActivePatterns =
    open LexerRegexDefinition
    open LexerTypes
    open LexerMappings
    open System.Text.RegularExpressions
    open System

    let matchToken patternToken input =
        let genericPattern = sprintf "\A(%s)((?s).*)"
        Regex.Match(input, genericPattern patternToken, RegexOptions.Multiline)
        |> fun mtch ->
            if mtch.Success then
                let token       = mtch.Groups.[1].Value
                let leftOver    = mtch.Groups.[2].Value
                let length      = token.Length |> Column
                (token, leftOver, length) |> Some
            else
                None

    let (|Whitespace|_|) input = 
        match matchToken whitespace input with
        | None -> None
        | Some (_ ,leftOver,length) -> Some (leftOver,length)

    let (|Newline|_|) input = 
        match matchToken newline input with
        | None -> None
        | Some (_ ,leftOver,length) -> Some (leftOver,length)


    let (|Comment|_|) input = 

        let (|LineComment|_|) input =
            match matchToken lineComment input with
            | None -> None
            | Some (comment,leftOver,length) -> 
                Some (comment |> Comment.LineComment |> TokenType.Comment,leftOver,length)
    
        let (|BlockComment|_|) input = 
            match matchToken blockComment input with
            | None -> None
            | Some (comment,leftOver,_) ->
                
                // we split the comment block such that we can handle the issue of the number of line the 
                // comment block takes, but also the new column value
                let commentSplitted = comment.Split( [| "\r\n"; "\r" ; "\n" |] ,StringSplitOptions.None)
                let numberOfLines = commentSplitted.Length - 1
                let lastPieceOfComment = commentSplitted.[numberOfLines]
                let column = lastPieceOfComment.Length |> Column
                let line = numberOfLines |> Line

                Some 
                    (comment |> Comment.BlockComment |> TokenType.Comment, leftOver, line, column )

        
        match input with
        | LineComment (comment,leftOver,length) -> Some (comment,leftOver, Line 0, length)
        | BlockComment (comment, leftOver, line, column) -> Some (comment, leftOver, line, column)
        | _ -> None

   
    let (|Separator|_|) input = 
        match matchToken separators input with
        | None -> None
        | Some (separator,leftOver,length) -> 
            Some (mapSeps.[separator] |> TokenType.Separator ,leftOver,length)
        

    let (|Operator|_|) input =
        match matchToken operators input with
        | None -> None
        | Some (operator,leftOver,length) -> 
            Some (mapOps.[operator] |> TokenType.Operator ,leftOver,length)
        

    let (|IdentifierOrKeyword|_|) input = 
        match matchToken identifierOrKeyword input with
        | None -> None
        | Some (idOrKey,leftOver,length) -> 
            match mapKeywords.TryFind(idOrKey) with
            | Some keyword  -> keyword |> TokenType.Keyword
            | None          -> idOrKey |> Ident |> TokenType.Identifier
            |> fun data -> Some (data,leftOver,length)


    let (|Literal|_|) input =

        let (|Int|_|) input =
            match matchToken int input with
            | None -> None
            | Some (dataInt,leftOver,length) -> 
                Some (Int32.Parse(dataInt) |> Literal.Int ,leftOver,length)

        let (|Float|_|) input =
            match matchToken float input with
            | None -> None
            | Some (dataFloat,leftOver,length) -> 
                Some (Double.Parse(dataFloat) |> Literal.Float ,leftOver,length)

        let (|Unit|_|) input =
            match matchToken unit input with
            | None -> None
            | Some (_ ,leftOver,length) -> 
                Some (Literal.Unit, leftOver,length)

        let (|String|_|) input =
            match matchToken string input with
            | None -> None
            | Some (dataString,leftOver,length) -> 
                Some (Literal.String dataString ,leftOver,length)

        let (|Bool|_|) input =
            match matchToken operators input with
            | None -> None
            | Some (dataBool,leftOver,length) ->
                Some ( Boolean.Parse(dataBool) |> Literal.Bool ,leftOver,length)

        match input with
        | Int       (data,leftOver,length)
        | Float     (data,leftOver,length) 
        | Unit      (data,leftOver,length)
        | String    (data,leftOver,length)
        | Bool      (data,leftOver,length) -> Some (data |> TokenType.Literal,leftOver,length)
        | _  -> None 


    let (|EndOfFile|_|) input = 
        match matchToken eof input with
        | None -> None
        | Some (_ ,leftOver,length) -> Some (leftOver,length) // Might want to just return Some ()


module LexerTokenization =
    open LexerTypes     
    open LexerActivePatterns  

    let sumColumn (Column c1) (Column c2) = c1 + c2 |> Column
    let sumLine (Line l1) (Line l2) = l1 + l2 |> Line


    type LexerState =
        {
            InversedTokens  : Token list
            CurrentLocation : Location
        }
    /// Order is important as the following rules are applied :
    /// 1) Longest match first
    /// 2) for a longest match the first regex in pattern matching that can match determines the token type

    let lexing input =
        let rec lexToken input state =
            match input with
            | Whitespace            (leftOver,length) ->
                let newState = 
                    { state with
                        CurrentLocation = 
                            { state.CurrentLocation with    
                                Column = sumColumn (state.CurrentLocation.Column) length
                            }
                    } 

                lexToken leftOver newState

            | Newline               (leftOver,_) ->
                let newState = 
                    { state with
                        CurrentLocation = 
                            { state.CurrentLocation with    
                                Line    = sumLine (state.CurrentLocation.Line) (Line 1)
                                Column  = Column 1
                            }
                    } 
                lexToken leftOver newState



            | Comment               (token,leftOver,line , column) ->
                let newColumn =
                    let (Line line) = line 
                    if line = 0 then
                        sumColumn (state.CurrentLocation.Column) column
                    else
                        column

                let token =
                    {
                        TokenType   = token
                        Location    = state.CurrentLocation
                    }
                let newState = 
                    { state with
                        InversedTokens  = token::state.InversedTokens
                        CurrentLocation = 
                            { state.CurrentLocation with    
                                Line   = sumLine (state.CurrentLocation.Line) line
                                Column = newColumn
                            }
                    } 
                lexToken leftOver newState


            | Separator             (token,leftOver,length)       
            | Operator              (token,leftOver,length)
            | IdentifierOrKeyword   (token,leftOver,length)
            | Literal               (token,leftOver,length) ->
                let token =
                    {
                        TokenType   = token
                        Location    = state.CurrentLocation
                    }
                let newState = 
                    { state with
                        InversedTokens  = token::state.InversedTokens
                        CurrentLocation = 
                            { state.CurrentLocation with    
                                Column = sumColumn (state.CurrentLocation.Column) length
                            }
                    } 
                lexToken leftOver newState

            | EndOfFile             _ -> state.InversedTokens |> List.rev
            
            | _ -> failwith "unexpected stuff"


        let initState = 
            {
                CurrentLocation = 
                    {
                        Line = Line 1
                        Column = Column 1
                    }
                InversedTokens = []
            }

        lexToken input initState

    let input = 
        "(* 
Some Multiline comment
 *)
let id //some comment*)a = a"

    let res = lexing input
