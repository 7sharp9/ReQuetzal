//based on algorithm_w from https://github.com/tomprimozic/type-systems/tree/master/algorithm_w

//This implementation uses several optimizations over the naive implementation. 
//Instead of explicit substitutions when unifying types, it uses updateable references. 
//It also tags unbound type variables with levels or ranks to optimize generalizations of let bindings, 
//a technique first described by Didier Rémy [1]. A very eloquent description of the ranked type variables 
//algorithm and associated optimizations was written by Oleg Kiselyov, see http://okmij.org/ftp/ML/generalization.html

//Also added to this is row polymorphism

module RowPoly
open System
open Persistent
type name = string

type expr =
      ///variable
    | Var of name
      ///application
    | Call of expr * expr list
      //abstraction
    | Fun of name list * expr
      //let
    | Let of name * expr * expr

      /// selecting value of label: `r.a`
    | RecordSelect of expr * name
      /// extending a record: `{a = 1 | r}`
    | RecordExtend of name * expr * expr
      /// deleting a label: `{r - a}`
    | RecordRestrict of expr * name
      /// empty record: `{}`
    | RecordEmpty

type id = int
type level = int

type ty =
      ///type constant: `int` or `bool`
    | TConst of name   
      ///type application: `list[int]`
    | TApp of ty * ty list
      ///function type: `(int, int) -> int`
    | TArrow of ty list * ty
      ///type variable
    | TVar of tvar ref

      /// record type: `{<...>}`
    | TRecord of row
      /// empty row: `<>`
    | TRowEmpty
      /// row extension: `<a : _ | ...>`
    | TRowExtend of name * ty * row

    /// The kind of rows - empty row, row variable, or row extension
and row = ty

and tvar =
    | Unbound of id * level
    | Link of ty
    | Generic of id

module exp =
    let toString expr : string =
        let rec tostringRec isSimple exp =
            match exp with
            | Var name -> name
            | Call(fnExpr, argList) ->
                tostringRec true fnExpr + "(" + String.concat ", " (List.map (tostringRec false) argList) + ")"
            | Fun(paramList, bodyExpr) ->
                    let funStr =
                        sprintf "fun %s -> %s" (String.concat " " paramList)  (tostringRec false bodyExpr)
                    if isSimple then sprintf "(%s)" funStr else funStr
            | Let(varName, valueExpr, bodyExpr) ->
                    let letStr =
                        sprintf "let %s = %s in %s" varName  (tostringRec false valueExpr) (tostringRec false bodyExpr)
                    if isSimple then sprintf "(%s)" letStr else letStr
            | RecordEmpty -> "{}"
            | RecordSelect(recordExpr, label) ->
                sprintf "%s.%s" (tostringRec true recordExpr) label
            | RecordRestrict(recordExpr, label) ->
                sprintf "{%s - %s }"  (tostringRec false recordExpr) label
            | RecordExtend(label, expr, recordExpr) ->
                    let rec appendElements str = function
                        | RecordEmpty -> str
                        | RecordExtend(label, expr, recordExpr) ->
                            appendElements (str + ", " + label + " = " + tostringRec false expr) recordExpr
                        | other_expr -> str + " | " + tostringRec false other_expr
                    "{" + appendElements (label + " = " + tostringRec false expr) recordExpr + "}"
        tostringRec false expr

module ty =
    let toString ty : string =
        let mutable idNameMap = PersistentHashMap.empty
        let count = ref 0
        let nextName () =
            let i = !count
            incr count
            let name =
                match i with
                | i when i >= 26 -> string(char (97 + i % 26)) + string (i / 26)
                | _ -> string(char (97 + i % 26))
            name
        let rec f isSimple = function
            | TConst name -> name
            | TApp(ty, tyArgList) ->
                    f true ty + "[" + String.concat ", " (tyArgList |> List.map (f false) ) + "]"
            | TArrow(paramTyList, returnTy) ->
                    let arrowTyStr =
                        match paramTyList with
                        | [singleTy] ->
                                let paramTyStr = f true singleTy
                                let returnTyStr = f false returnTy
                                sprintf "%s -> %s" paramTyStr returnTyStr
                        | _ ->
                                let paramTyListStr = String.concat ", " (paramTyList |> List.map (f false) )
                                let returnTyStr = f false returnTy
                                sprintf "(%s) -> %s" paramTyListStr returnTyStr
                    
                    if isSimple then sprintf "(%s)" arrowTyStr else arrowTyStr
            | TVar {contents = Generic id} -> 
                        
                            match idNameMap |> PersistentHashMap.tryFind id with
                            | Some ty -> ty
                            | None ->
                                let name = nextName()
                                idNameMap <- idNameMap |> PersistentHashMap.set id name 
                                name
                    
            | TVar {contents = Unbound(id, _)} -> "_" + string id
            | TVar {contents = Link ty} -> f isSimple ty
            | TRecord row_ty ->
                sprintf "{%s}" (f false row_ty)
            | TRowEmpty -> ""
            | TRowExtend(label, ty, rowTy) ->
                //replace with fold over stringbuilder?
                let rec appendElements str ty =
                    match ty with
                    | TRowEmpty -> str
                    | TRowExtend(label, ty, rowTy) ->
                            appendElements (str + ", " + label + " : " + f false ty) rowTy
                    | TVar {contents = Link ty} -> appendElements str ty
                    | otherTy -> str + " | " + f false otherTy
                appendElements (label + " : " + f false ty) rowTy
        
        let tyStr = f false ty
        if !count > 0 then
            let varNames =
                idNameMap
                |> PersistentHashMap.toSeq
                |> Seq.map (fun kv  -> kv.Value )
                |> Seq.sort
            sprintf "forall[%s] %s" (String.concat " " varNames) tyStr
        else
            tyStr

let currentId = ref 0

let nextId() =
    let id = !currentId in
    currentId := id + 1
    id

let resetId() = currentId := 0

let newVar level = TVar (ref (Unbound(nextId (), level)))
let newGenVar() = TVar (ref (Generic(nextId ())))

//refactor to use mutable persistanthashmap or state monad?
module Env =
    type env = Map<String, ty>
    let empty = Map.empty : env
    let extend env name ty =
        env |> Map.add name ty
    let lookup env name =
        env |> Map.find name

let occursCheckAdjustLevels tvarId tvarLevel ty =
    let rec occursRec ty =
        match ty with
        | TVar {contents = Link ty} -> occursRec ty
        | TVar {contents = Generic _} as tvar->
            ()//failwithf "%A should not be matched here for ty: %A" tvar ty
        | TVar ({contents = Unbound(otherId, otherLevel)} as otherTvar) ->
            if otherId = tvarId then
                failwithf "recursive types are not allowed: %A" ty
            else
                if otherLevel > tvarLevel then
                    otherTvar := Unbound(otherId, tvarLevel)
                else ()
        | TApp(ty, tyArgList) ->
                occursRec ty
                List.iter occursRec tyArgList
        | TArrow(paramTyList, returnTy) ->
                List.iter occursRec paramTyList
                occursRec returnTy
        | TConst _ -> ()
        //Records
        | TRecord row -> occursRec row
        | TRowExtend(_label, fieldTy, row) ->
            occursRec fieldTy
            occursRec row
        | TRowEmpty -> ()
    occursRec ty

let rec unify (ty1) (ty2) =
    if ty1 = ty2 then () else
    match (ty1, ty2) with
        | TConst(name1), TConst(name2) when name1 = name2 -> ()
        | TApp(ty1, tyArgList1), TApp(ty2, tyArgList2) ->
                unify ty1 ty2
                List.iter2 unify tyArgList1 tyArgList2
        | TArrow(paramTyList1, returnTy1), TArrow(paramTyList2, returnTy2) ->
                List.iter2 unify paramTyList1 paramTyList2
                unify returnTy1 returnTy2
        | TVar {contents = Link ty1}, ty2
        | ty1, TVar {contents = Link ty2} -> unify ty1 ty2
        | TVar {contents = Unbound(id1, _)}, TVar {contents = Unbound(id2, _)} when id1 = id2 ->
                failwithf "Error: There should only a single instance of a particular type variable"
        | TVar ({contents = Unbound(id, level)} as tvar), ty
        | ty, TVar ({contents = Unbound(id, level)} as tvar) ->
                occursCheckAdjustLevels id level ty
                tvar := Link ty
        //records
        | TRecord row1, TRecord row2 -> unify row1 row2
        | TRowEmpty, TRowEmpty -> ()
        | TRowExtend(label1, fieldTy1, restRow1), (TRowExtend _ as row2) ->
            let restRow1TvarRefOption =
                match restRow1 with
                | TVar ({contents = Unbound _} as tvar_ref) -> Some tvar_ref
                | _ -> None
            let restRow2 = rewriteRow row2 label1 fieldTy1
            match restRow1TvarRefOption with
            | Some {contents = Link _} -> failwithf "Error: recursive row type of %A" restRow1
            | _ -> ()
            unify restRow1 restRow2
        | _, _ ->
            failwithf "cannot unify types %s and %s"  (ty.toString ty1) (ty.toString ty2)

and rewriteRow row2 label1 fieldTy1 =
    match row2 with
    | TRowEmpty -> failwithf "row does not contain label %s" label1
    | TRowExtend(label2, fieldTy2, restRow2) when label2 = label1 ->
            unify fieldTy1 fieldTy2 ;
            restRow2
    | TRowExtend(label2, fieldTy2, restRow2) ->
            TRowExtend(label2, fieldTy2, rewriteRow restRow2 label1 fieldTy1)
    | TVar {contents = Link row2} -> rewriteRow row2 label1 fieldTy1
    | TVar ({contents = Unbound(id, level)} as tvar) ->
            let restRow2 = newVar level
            let ty2 = TRowExtend(label1, fieldTy1, restRow2)
            tvar := Link ty2
            restRow2
    | _ -> failwithf "row type expected"

let rec generalize level ty =
    match ty with
    | TVar {contents = Unbound(id, otherLevel)} when otherLevel > level ->
        TVar (ref (Generic id))
    | TApp(ty, tyArgList) ->
        TApp(generalize level ty, tyArgList |> List.map (generalize level) )
    | TArrow(paramTyList, returnTy) ->
        TArrow(paramTyList |> List.map (generalize level), generalize level returnTy)
    | TVar {contents = Link ty } ->
        generalize level ty
    | TRecord row ->
        TRecord (generalize level row)
    | TRowExtend(label, fieldTy, row) ->
        TRowExtend(label, generalize level fieldTy, generalize level row)
    | TVar {contents = Generic _ }
    | TVar {contents = Unbound _ }
    | TConst _
    | TRowEmpty as ty -> ty

let instantiate level ty =
    //could easily just be a dictionary/ Hashtable, or even state monad
    let mutable idVarMap = PersistentHashMap.empty
    let rec instantiateRec ty =
        match ty with
        | TConst _ -> ty
        | TVar {contents = Link ty} -> instantiateRec ty
        | TVar {contents = Generic id} ->
                match idVarMap |> PersistentHashMap.tryFind id with
                | Some ty -> ty
                | None ->
                    let var = newVar level
                    idVarMap <- idVarMap |> PersistentHashMap.set id var
                    var
        | TVar {contents = Unbound _} -> ty
        | TApp(ty, tyArgList) ->
                TApp(instantiateRec ty, List.map instantiateRec tyArgList)
        | TArrow(paramTyList, returnTy) ->
                TArrow(List.map instantiateRec paramTyList, instantiateRec returnTy)
        | TRecord row -> TRecord (instantiateRec row)
        | TRowEmpty -> ty
        | TRowExtend(label, fieldTy, row) ->
            TRowExtend(label, fieldTy, instantiateRec row)
    instantiateRec ty

let rec matchFunTy numParams ty =
    match ty with
    | TArrow(paramTyList, returnTy) ->
            if List.length paramTyList <> numParams then
                failwithf "unexpected number of arguments, expected %i but was %i" numParams (List.length paramTyList)
            else paramTyList, returnTy
    | TVar {contents = Link ty} -> matchFunTy numParams ty
    | TVar ({contents = Unbound(_id, level)} as tvar) ->
            let paramTyList = 
                let rec loop i =
                    match i with
                    | 0 -> []
                    | n -> newVar level :: loop (n - 1)
                loop numParams
            let returnTy = newVar level
            tvar := Link (TArrow(paramTyList, returnTy))
            paramTyList, returnTy
    | other -> failwithf "expected a function but was a %A" other

let rec infer env level exp =
        match exp with
        // x : σ ∈ Γ
        // --−−−−−−−
        // Γ ⊢ x : σ
        | Var name ->
                try instantiate level (Env.lookup env name)
                with ex -> failwithf "variable %s not found" name
        ///  Γ , x : τ ⊢ e : τ′
        /// −−−−−−−−−−−−−−------
        /// Γ ⊢ λ x . e : τ → τ′
        | Fun(paramList, bodyExpr) ->
                let paramTyList = paramList |> List.map (fun _ -> newVar level) 
                let fnEnv =
                    List.fold2 (fun env paramName paramTy -> Env.extend env paramName paramTy) env paramList paramTyList
                let returnTy = infer fnEnv level bodyExpr
                TArrow(paramTyList, returnTy)
        // Γ ⊢ e0 : τ → τ′   Γ ⊢ e1 : τ
        // −--------−−−−−−−−−−−−−−−−−−−−
        //         Γ ⊢ e0(e1) : τ′
        | Let(varName, valueExpr, bodyExpr) ->
                let varTy = infer env (level + 1) valueExpr
                let generalizedTy = generalize level varTy
                infer (Env.extend env varName generalizedTy) level bodyExpr
        // Γ ⊢ e0 : τ → τ′   Γ ⊢ e1 : τ
        // −--------−−−−−−−−−−−−−−−−−−−−
        //        Γ ⊢ e0(e1) : τ′
        | Call(fnExpr, argList) ->
                let paramTyList, returnTy =
                    matchFunTy (List.length argList) (infer env level fnExpr)
                List.iter2
                    (fun paramTy argExpr -> unify paramTy (infer env level argExpr))
                    paramTyList argList
                returnTy
        //TODO add empty record rule
        | RecordEmpty -> TRecord TRowEmpty
        //TODO add record select rule 
        | RecordSelect(recordExpr, label) ->
                /// inlined code for Call of function with type `forall[a r] {label : a | r} -> a`
                let restRowTy = newVar level
                let fieldTy = newVar level
                let paramTy = TRecord (TRowExtend(label, fieldTy, restRowTy))
                let returnTy = fieldTy
                unify paramTy (infer env level recordExpr)
                returnTy
        //TODO: add rule for record restriction
        | RecordRestrict(recordExpr, label) ->
                /// inlined code for Call of function with type `forall[a r] {label : a | r} -> {r}`
                let restRowTy = newVar level
                let fieldTy = newVar level
                let paramTy = TRecord (TRowExtend(label, fieldTy, restRowTy))
                let returnTy = TRecord restRowTy
                unify paramTy (infer env level recordExpr)
                returnTy
        //TODO: Add rule for record extend
        | RecordExtend(label, expr, recordExpr) ->
                /// inlined code for Call of function with type `forall[a r] (a, {r}) -> {label : a | r}`
                let restRowTy = newVar level
                let fieldTy = newVar level
                let param1Ty = fieldTy
                let param2Ty = TRecord restRowTy
                let returnTy = TRecord (TRowExtend(label, fieldTy, restRowTy))
                unify param1Ty (infer env level expr)
                unify param2Ty (infer env level recordExpr)
                returnTy




let basicEnv =
    let env = ref Env.empty
    let extend name ty =
        env := Env.extend !env name ty
    
    extend "zero" (TConst "int")
    extend "one" (TConst "int")
    extend "true" (TConst "bool")
    extend "false" (TConst "bool")
    ///let choose (a:'a) (b:'a) = a
    extend "choose" (TArrow([TVar {contents = Generic 0}; TVar {contents = Generic 0}], TVar {contents = Generic 0}))
    extend "succ" (TArrow([TConst("int")], TConst("int")))
    extend "id" (TArrow([TVar {contents = Generic 0}], TVar {contents = Generic 0}))
    !env

    

///let f = fun x y -> g(x, y) in f(a, b)
let example1 =
    Fun([("a")], Var(("a") ))

///compose or (>>)
let example2 =
    Let("compose",
        Fun(["f"], 
            Fun(["g"], 
                Fun (["x"], 
                    Call(Var "g",[Call(Var "f", [Var "x"])])
                )
            )
        ), Var "compose"
    )
///fun (a,b) -> a
let example3 =
    Fun(["a"; "b"], Var(("a") ))

///let f a b = a
let example4 =
    Fun( ["a"], Fun(["b"], Var "a"))

///fun x -> let y = x in y
let example5 = 
    Fun(["x"], Let("y", Var "x", Var "y"))

///{ }
/// :{ }
let example6 =
    RecordEmpty

///{a = one}"
///: {a : int}
let example7 =    
    RecordExtend("a", Var "one", RecordEmpty)


///{a = one, b = true}
///: {a : int, b : bool}
let example8 =
    RecordExtend("a", Var("one"), RecordExtend("b", Var("true"), RecordEmpty) )

///{b = true, a = one}
///: {b : bool, a : int}
let example9 =
    RecordExtend("b", Var("true"), RecordExtend("a", Var("one"), RecordEmpty))

///{a = one, b = true}.a
///: int
let example10 = 
    RecordSelect(RecordExtend("a", Var("one"), RecordExtend("b", Var("true"), RecordEmpty)), "a")

///{a = one, b = true}.b
///: bool
let example11 = 
    RecordSelect(RecordExtend("a", Var("one"), RecordExtend("b", Var("true"), RecordEmpty)), "b")

///{f = fun x -> x}
///: {f : a -> a}
let example12 =
    RecordExtend("f", Fun(["x"], Var("x")), RecordEmpty)

///let r = {a = id, b = succ} in choose(r.a, r.b)
let example13 =
    Let("r", (RecordExtend("a", Var("id"), RecordExtend("b", Var("succ"), RecordEmpty))),
        Call(Var("choose"), [RecordSelect(Var("r"), "a"); RecordSelect(Var("r"), "b")])
    )

///let r = {a = id, b = fun x -> x} in choose(r.a, r.b)
let example14 =
    Let("r", (RecordExtend("a", Var("id"), RecordExtend("b", Fun(["x"], Var("x")), RecordEmpty))), 
        Call(Var("choose"), [RecordSelect(Var("r"), "a"); RecordSelect(Var("r"), "b")])
    )


let recordRecurse = 
    //fun r -> choose({x = zero | r}, {y = one | r})
    Fun(["r"], Call( Var("choose"), [RecordExtend("x", Var("zero"), Var("r")); RecordExtend("y", Var("one"), Var("r")) ]))
    //This should error, recursive row types are not allowed

