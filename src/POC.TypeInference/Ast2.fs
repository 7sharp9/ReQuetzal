//based on algorithm_w from https://github.com/tomprimozic/type-systems/tree/master/algorithm_w

//This implementation uses several optimizations over the naive implementation. 
//Instead of explicit substitutions when unifying types, it uses updateable references. 
//It also tags unbound type variables with levels or ranks to optimize generalizations of let bindings, 
//a technique first described by Didier Rémy [1]. A very eloquent description of the ranked type variables 
//algorithm and associated optimizations was written by Oleg Kiselyov, see http://okmij.org/ftp/ML/generalization.html

module Ast2
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

and tvar =
    | Unbound of id * level
    | Link of ty
    | Generic of id

let stringOfExpr expr : string =
    let rec stringOfExprRec isSimple exp =
        match exp with
        | Var name -> name
        | Call(fnExpr, argList) ->
            stringOfExprRec true fnExpr + "(" + String.concat ", " (List.map (stringOfExprRec false) argList) + ")"
        | Fun(paramList, bodyExpr) ->
                let funStr =
                    sprintf "fun %s -> %s" (String.concat " " paramList)  (stringOfExprRec false bodyExpr)
                if isSimple then sprintf "(%s)" funStr else funStr
        | Let(varName, valueExpr, bodyExpr) ->
                let letStr =
                    sprintf "let %s = %s in %s" varName  (stringOfExprRec false valueExpr) (stringOfExprRec false bodyExpr)
                if isSimple then sprintf "(%s)" letStr else letStr
    stringOfExprRec false expr

let stringOfTy ty : string =
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

exception Error of string
let error msg = raise (Error msg)


module Env =
    type env = Map<String, ty>
    let empty = Map.empty : env
    let extend env name ty =
        env |> Map.add name ty
    let lookup env name =
        env |> Map.find name

let occursCheckAdjustLevels tvarId tvarLevel ty =
    let rec loop ty =
        match ty with
        | TVar {contents = Link ty} -> loop ty
        | TVar {contents = Generic _} -> assert false
        | TVar ({contents = Unbound(otherId, otherLevel)} as otherTvar) ->
                if otherId = tvarId then
                    error "recursive types"
                else
                    if otherLevel > tvarLevel then
                        otherTvar := Unbound(otherId, tvarLevel)
                    else ()
        | TApp(ty, tyArgList) ->
                loop ty
                List.iter loop tyArgList
        | TArrow(paramTyList, returnTy) ->
                List.iter loop paramTyList
                loop returnTy
        | TConst _ -> ()
    loop ty

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
                assert false //There is only a single instance of a particular type variable
        | TVar ({contents = Unbound(id, level)} as tvar), ty
        | ty, TVar ({contents = Unbound(id, level)} as tvar) ->
                occursCheckAdjustLevels id level ty
                tvar := Link ty
        | _, _ -> error (sprintf "cannot unify types %s and %s"  (stringOfTy ty1) (stringOfTy ty2))

let rec generalize level ty =
    match ty with
    | TVar {contents = Unbound(id, otherLevel)} when otherLevel > level ->
            TVar (ref (Generic id))
    | TApp(ty, tyArgList) ->
            TApp(generalize level ty, List.map (generalize level) tyArgList)
    | TArrow(paramTyList, returnTy) ->
            TArrow(List.map (generalize level) paramTyList, generalize level returnTy)
    | TVar {contents = Link ty } -> generalize level ty
    | TVar {contents = Generic _ }
    | TVar {contents = Unbound _ }
    | TConst _ as ty -> ty

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
    instantiateRec ty

let rec matchFunTy numParams ty =
    match ty with
    | TArrow(paramTyList, returnTy) ->
            if List.length paramTyList <> numParams then
                error (sprintf "unexpected number of arguments, expected %i but was %i" numParams (List.length paramTyList) )
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
    | other -> error (sprintf "expected a function but was a %A" other)

let rec infer env level exp =
    match exp with
    | Var name ->
            try instantiate level (Env.lookup env name)
            with _ex -> failwithf "variable %s not found" name
        
    | Fun(paramList, bodyExpr) ->
            let paramTyList = paramList |> List.map (fun _ -> newVar level) 
            let fnEnv =
                List.fold2 (fun env paramName paramTy -> Env.extend env paramName paramTy) env paramList paramTyList
            let returnTy = infer fnEnv level bodyExpr
            TArrow(paramTyList, returnTy)
    | Let(varName, valueExpr, bodyExpr) ->
            let varTy = infer env (level + 1) valueExpr
            let generalizedTy = generalize level varTy
            infer (Env.extend env varName generalizedTy) level bodyExpr
    | Call(fnExpr, argList) ->
            let paramTyList, returnTy =
                matchFunTy (List.length argList) (infer env level fnExpr)
            List.iter2
                (fun paramTy argExpr -> unify paramTy (infer env level argExpr))
                paramTyList argList
            returnTy

///let f = fun x y -> g(x, y) in f(a, b)
let example1 =
    Fun([("a")], Var(("a") ))

///(>>)
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


