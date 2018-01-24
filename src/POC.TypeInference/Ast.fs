module rec Ast
open System
open System.Collections

//An implementation of the Hindley Milner type checking algorithm
//based on the Scala code by Andrew Forrest, the Perl code by
//Nikita Borisov and the paper "Basic Polymorphic Typechecking"
//by Cardelli.

type exp =
    ///Identifier
    | Ident  of name:string
    ///Lambda abstraction
    | Lambda of name:string * body:exp
    ///Function application
    | Apply  of func:exp    * argument:exp
    //Let binding
    | Let    of name:string * definition:exp * body:exp
    //Recursive Let binding
    | LetRec of name:string * definition:exp * body:exp

module exp =
    let rec toString (exp : exp) =
        match exp with
        | Ident name -> name
        | Lambda(v, body) -> sprintf "fun %s -> %s" v (toString body)
        | Apply(fn, arg) -> sprintf "%s %s" (toString fn) (toString arg)
        | Let(v, def, body) -> sprintf "let %s = %s in %s" v (toString def) (toString body)
        | LetRec(v, def, body) -> sprintf "let rec %s = %s in %s" v (toString def) (toString body)

///A type variable standing for an arbitrary type.
///All type variables have a unique id, but names are only assigned lazily, when required.
type tyvar = 
  { id : int;
    mutable instance : ty option
    ///Names are allocated to TypeVariables lazily, so that only TypeVariables present
    mutable name: string option
  }

///An n-ary type constructor which builds a new type from old
and tyop =
  { name : string
    types: ty list
  }

and ty =
  | TypeVariable of tyvar
  | TypeOperator of tyop

let nextVariableId = ref 0

let makeVariable () =
  let newVar = { id = !nextVariableId; instance = None; name = None }
  nextVariableId := !nextVariableId + 1
  TypeVariable(newVar)

let nextUniqueName = ref "a"

let variableName (v: tyvar) : string =
  match v with
  | { name = Some(name) } -> name
  | { name = None } ->
      let newVarName = !nextUniqueName
      v.name <- Some(newVarName)
      nextUniqueName := string (int (!nextUniqueName).[0] + 1)
      newVarName

module ty =
    let rec toString ty =
      match ty with
      | TypeVariable( { instance = Some(instance)} ) -> toString instance
      | TypeVariable( { instance = None } as v ) -> variableName v
      | TypeOperator( { name = tyopName; types = tyopTypes } ) ->
          match List.length tyopTypes with
          | 0 -> tyopName
          | 2 -> sprintf "(%s %s %s)"
                   (toString (List.item 0 tyopTypes))
                   tyopName
                   (toString (List.item 1 tyopTypes ))
          | _ -> sprintf "%s %s" tyopName
                   (String.concat " " (List.map toString tyopTypes))

type env = (string * ty) list

let makeFunctionType fromType toType =
  TypeOperator({ name = "->"; types = [fromType; toType] })
let intType = TypeOperator({ name = "int"; types = [] })
let boolType = TypeOperator({ name = "bool"; types = [] })

/// Returns the currently defining instance of t.
/// As a side effect, collapses the list of type instances. The function Prune
/// is used whenever a type expression has to be inspected: it will always
/// return a type expression which is either an uninstantiated type variable or
/// a type operator; i.e. it will skip instantiated variables, and will
/// prune them from expressions to remove long chains of instantiated variables.
let rec prune t =
  match t with
  | TypeVariable({ instance = Some(instance) } as v) ->
      let newInstance = prune instance
      v.instance <- Some(newInstance);
      newInstance
  | _ -> t

let rec occursInType v t2 =
  match prune t2 with
  | pruned when pruned = v -> true
  | TypeOperator({ types = types }) -> occursIn v types
  | _ -> false

and occursIn t types =
  List.exists (fun t2 -> occursInType t t2) types

let isIntegerLiteral (name:string) =
  match (try Some(int name) with | _ex -> None) with
  | None -> false
  | Some(_) -> true

let isGeneric v nonGeneric =
  not (nonGeneric |> Set.contains v )

///Makes a copy of a type expression.
///The type t is copied. The the generic variables are duplicated and the
///nonGeneric variables are shared.
let fresh t nonGeneric =
  let table = Hashtable()
  let rec loop tp =
    match prune tp with
    | TypeVariable(_) as p ->
      if isGeneric p nonGeneric then
        match table.ContainsKey p with
        | false ->
          let newVar = makeVariable()
          table.Add(p, newVar)
          newVar
        | true -> table.[p] :?> ty
      else p
    | TypeOperator({ types = tyopTypes } as op) ->
        TypeOperator({ op with types = List.map loop tyopTypes })
  loop t

///Get the type of identifier name from the type environment env
let getType name env nonGeneric =
  match env |> List.tryFind (fun (n, _) -> n = name ) with
  | Some(_name, var) -> fresh var nonGeneric
  | None ->
      if isIntegerLiteral name then intType
      else failwithf "Undefined symbol %s" name

///Unify the two types t1 and t2. Makes the types t1 and t2 the same.
let rec unify t1 t2 =
  match prune t1, prune t2 with
  | (TypeVariable(v) as a), b ->
      if a <> b then
        if occursInType a b then failwith "Recursive unification"
        v.instance <- Some(b)
  | (TypeOperator(_) as a), (TypeVariable(_) as b) -> unify b a
  | (TypeOperator({ name = name1; types = types1 }) as a), (TypeOperator({ name = name2; types = types2 }) as b) ->
    if (name1 <> name2 || List.length types1 <> List.length types2)
    then failwith (sprintf "Type mismatch %s != %s" (ty.toString a) (ty.toString b))
    ignore (List.map2 unify types1 types2)

///Computes the type of the expression given by node.
///The type of the node is computed in the context of the
///supplied type environment env. Data types can be introduced into the
///language simply by having a predefined set of identifiers in the initial
///environment. environment; this way there is no need to change the syntax or, more
///importantly, the type-checking program when extending the language.
let analyse exp env =
  let rec loop exp env nonGeneric =
    match exp with
    | Ident(name) -> getType name env nonGeneric
    | Apply(fn, arg) ->
      let funTy = loop fn env nonGeneric
      let argTy = loop arg env nonGeneric
      let retTy = makeVariable()
      unify (makeFunctionType argTy retTy) funTy
      retTy
    | Lambda(arg, body) ->
      let argTy = makeVariable()
      let newEnv = (arg, argTy) :: env
      let newNonGeneric = nonGeneric |> Set.add argTy
      let retTy = loop body newEnv newNonGeneric
      makeFunctionType argTy retTy
    | Let(v, defn, body) ->
      let defnTy = loop defn env nonGeneric
      loop body ( (v ,defnTy) :: env) nonGeneric
    | LetRec(v, defn, body) ->
      let newTy = makeVariable()
      let newEnv = (v, newTy) :: env
      let newNonGeneric = nonGeneric |> Set.add newTy
      let defnTy = loop defn newEnv newNonGeneric
      unify newTy defnTy
      loop body newEnv nonGeneric
  loop exp env Set.empty

let tryExp env (exp: exp) =
  Printf.printf "%s : " (Ast.exp.toString exp)
  let result =
    try
      let t = analyse exp env in
      ty.toString t
    with
    | ex -> ex.Message
  Printf.printf "%s\n\n" result

let myEnv =
    let var1 = makeVariable ()
    let var2 = makeVariable ()
    let pairTy = TypeOperator({ name = "*"; types = [var1; var2] })
    let var3 = makeVariable ()
    [ ("pair", makeFunctionType var1 (makeFunctionType var2 pairTy))
      ("true", boolType)
      ("cond", makeFunctionType boolType (makeFunctionType var3 (makeFunctionType var3 var3)))
      ("zero", makeFunctionType intType boolType)
      ("pred", makeFunctionType intType intType)
      ("times", makeFunctionType intType (makeFunctionType intType intType))
    ]

let examples =
  let pair = Apply(Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))), Apply(Ident("f"), Ident("true")))
  [ LetRec("factorial", (* letrec factorial = *)
      Lambda("n",    (* fn n => *)
        Apply(
          Apply(   (* cond (zero n) 1 *)
            Apply(Ident("cond"),     (* cond (zero n) *)
              Apply(Ident("zero"), Ident("n"))),
            Ident("1")),
          Apply(    (* times n *)
            Apply(Ident("times"), Ident("n")),
            Apply(Ident("factorial"),
              Apply(Ident("pred"), Ident("n")))
          )
        )
      ),      (* in *)
      Apply(Ident("factorial"), Ident("5"))
    );

    (* Should fail: *)
    (* fn x => (pair(x(3) (x(true))) *)
      Lambda("x",
        Apply(
          Apply(Ident("pair"),
            Apply(Ident("x"), Ident("3"))),
          Apply(Ident("x"), Ident("true"))));

    (* pair(f(3), f(true)) *)
      Apply(
        Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))),
        Apply(Ident("f"), Ident("true")));

    (* letrec f = (fn x => x) in ((pair (f 4)) (f true)) *)
      Let("f", Lambda("x", Ident("x")), pair);

    (* fn f => f f (fail) *)
      Lambda("f", Apply(Ident("f"), Ident("f")));

    (* let g = fn f => 5 in g g *)
      Let("g",
        Lambda("f", Ident("5")),
        Apply(Ident("g"), Ident("g")));

    (* example that demonstrates generic and non-generic variables: *)
    (* fn g => let f = fn x => g in pair (f 3, f true) *)
      Lambda("g",
           Let("f",
             Lambda("x", Ident("g")),
             Apply(
              Apply(Ident("pair"),
                  Apply(Ident("f"), Ident("3"))
              ),
              Apply(Ident("f"), Ident("true")))));

    (* Function composition *)
    (* fn f (fn g (fn arg (f g arg))) *)
      Lambda("f", Lambda("g", Lambda("arg", Apply(Ident("g"), Apply(Ident("f"), Ident("arg"))))))
  ] 