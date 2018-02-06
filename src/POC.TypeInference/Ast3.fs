module rec Ast3
open System
open System.Collections.Generic
open Persistent
open System.Data.Common
open System.Xml.Linq
open Microsoft.VisualBasic.CompilerServices

type name = string
type label = string

type exp = 
    | EVar of name
    | EPrim of prim
    | EApp of exp * exp
    | EAbs of name * exp
    | ELet of name * exp * exp

and prim =
    | Int of int
    | Bool of bool
    | Cond
    | RecordSelect of label
    | RecordExtend of label
    | RecordRestrict of label
    | RecordEmpty

type Typ =
    | TVar of name
    | TInt
    | TBool
    | TFun of Typ * Typ
    | TRecord of Typ
    | TRowEmpty
    | TRowExtend of label * Typ * Typ

type Scheme = Scheme of name list * Typ

type TypeEnv = Map<string, Scheme>

type Subst = Map<name,Typ>

module Typ =
    let freeTypeVariables (typ: Typ) =
        let rec ftvrec typ =
            match typ with
            | TVar name -> Set.singleton name
            | TInt -> Set.empty
            | TBool -> Set.empty
            | TFun(type1, type2) ->
                let free1 = ftvrec type1 
                let free2 = ftvrec type2
                Set.union free1 free2
            | TRecord typ -> ftvrec typ
            | TRowEmpty -> Set.empty
            | TRowExtend (_label, typ, row) ->
                let free1 = ftvrec row
                let free2 = ftvrec typ
                Set.union free1 free2
        ftvrec typ
    
    let apply subst typ =
        let rec apply subst typ =
            match typ with
            | TVar name ->
                match subst |> Map.tryFind name with
                | Some t -> t
                | None -> TVar name
            | TFun(type1, type2) ->
                TFun (apply subst type1, apply subst type2)
            | TRecord typ -> TRecord (apply subst typ)
            | TRowExtend (label, typ, row) ->
                TRowExtend(label, apply subst typ, apply subst row)
            | TInt | TBool | TRowEmpty -> typ
        apply subst typ

    let parens s =
        sprintf "( %s )" s
        
    let braces s =
        sprintf "{ %s }" s
    let toString ty =
        let rec parenType ty =
            match ty with
            |  TFun(_type1, _type2) -> parens (toString ty)
            | _ -> toString ty

        match ty with
            | TVar name -> name
            | TInt -> "Int"
            | TBool -> "Bool"
            | TFun(t, s) ->
                (parenType t) + " -> " + (toString s)
            | TRecord typ ->
                sprintf "record: { %s }" ( toString typ)
            | TRowEmpty -> "{ }"
            | TRowExtend (label, typ, row) ->
                sprintf "%s = %s | %s" label (toString typ) (toString row) 

module Scheme =
   let freeTypeVars (scheme: Scheme) =
       match scheme with
       | Scheme(variables, typ) ->
           let freeTypeVariables = Typ.freeTypeVariables typ
           freeTypeVariables - (Set.ofList variables)

   let apply (subst: Subst) (scheme: Scheme) =
       match scheme with
       | Scheme(variables, typ) ->
           let newSubst =
               variables
               |> List.fold (fun ns key -> ns |> Map.remove key) subst 
           let newTyp = typ |> Typ.apply newSubst
           Scheme(variables, newTyp)

module TypeEnv =
     let remove (var : string) (typEnv: TypeEnv)=
        typEnv.Remove var
    
     let freeTypeVars (typEnv: TypeEnv) =
        typEnv
        |> Seq.fold (fun state (KeyValue(_key ,value)) ->
               Set.union state (value |> Scheme.freeTypeVars)) Set.empty

     let apply (subst : Subst) (typEnv: TypeEnv) =
        typEnv
        |> Map.map (fun _k v -> v |> Scheme.apply subst)

module Subst =
    /// Apply `s1` to `s2` then merge the results
    let compose s1 s2 =
        let ns2 = Map.map (fun k (v : Typ) -> Typ.apply s1 v) s2
        Map.union ns2 s1

///generalize abstracts a type over all type variables which are free
/// in the type but not free in the given type environment.
let generalize (env : TypeEnv) (typ : Typ) =
    let result = Typ.freeTypeVariables typ - TypeEnv.freeTypeVars env
    let variables = Set.toList result
    Scheme(variables, typ)



let private currentId = ref 0

let nextId() =
    let id = !currentId
    currentId := id + 1
    id

let resetId() = currentId := 0

let newTyVar prefix =
    TVar ( sprintf "%s%i" prefix (nextId ()))


/// The instantiation function replaces all bound type variables in a
/// type scheme with fresh type variables.
let instantiate (ts : Scheme) =
    match ts with
    | Scheme(variables, typ) ->
        let nvars = variables |> List.map (fun _ -> newTyVar "a") 
        let subst = Map.ofSeq (Seq.zip variables nvars)
        typ |> Typ.apply subst

let rewriteRow (row: Typ) newLabel =
    match row with
    | TRowEmpty -> failwithf "label %s cannot be inserted" newLabel
    | TRowExtend(label, fieldTy, rowTail) when newLabel = label ->
        (fieldTy, rowTail, Map.empty) //nothing to do
    | TRowExtend(label, fieldTy, rowTail) ->
        match rowTail with
        | TVar alpha ->
             let beta  = newTyVar "r"
             let gamma = newTyVar "a"
             gamma, TRowExtend(label, fieldTy, beta), Map.singleton alpha (TRowExtend(newLabel, gamma, beta))     
        | _otherwise ->
            let (fieldTy', rowTail', s) = rewriteRow rowTail newLabel
            fieldTy', TRowExtend(label, fieldTy, rowTail'), s
    | _ -> failwithf "Unexpected type: %A" row

let varBind u t =
    match t with
    | TVar _name -> Map.empty
    | _ when t |> Typ.freeTypeVariables |> Set.contains u ->
        failwithf "Occur check fails: %s vs %A" u t
    | _ -> Map.singleton u t

let rec unify (t1 : Typ) (t2 : Typ) : Subst =
    match t1, t2 with
    | TFun (l1, r1), TFun (l2, r2) ->
        let s1 = unify l1 l2
        let s2 = unify (Typ.apply s1 r1) (Typ.apply s1 r2)
        Subst.compose s1 s2
    | TVar name, typ
    | typ, TVar name -> varBind name typ
    | TInt, TInt -> Map.empty
    | TBool, TBool -> Map.empty
    | TRecord row1, TRecord row2 ->
         unify row1 row2
    | TRowEmpty, TRowEmpty -> Map.empty
    | TRowExtend(label1, fieldTyp1, rowTail1), (TRowExtend(_,_,_) as row2) ->
        let fieldTy2, rowTail2, theta1 = rewriteRow row2 label1
        let rec loop ty =
            match ty with
            | TVar name -> [], Some name
            | TRowEmpty -> [], None
            | TRowExtend(label, fieldType, row) ->
                let ls, mv = loop row
                (label, fieldType) :: ls, mv
            | _ -> failwithf "invalid row tail %A" ty
        let result = loop rowTail1
        match snd result with
        | Some tv when theta1 |> Map.containsKey tv ->
            failwithf "recursive row type"
        | _ -> 
            let theta2 = unify (Typ.apply theta1 fieldTyp1) (Typ.apply  theta1 fieldTy2)
            let subst = Subst.compose theta1 theta2
            let theta3 = unify (Typ.apply subst rowTail1) (Typ.apply subst rowTail2)
            Subst.compose subst theta3
    | _ -> failwithf "Types do not unify: %A vs %A" t1 t2


let rec ti (env : TypeEnv) (exp : exp) : Subst * Typ =
    match exp with
    | EVar name ->
        match env |> Map.tryFind name with
        | None -> failwithf "Unbound variable: %s" name
        | Some sigma ->
            let t = instantiate sigma
            Map.empty, t
    | EPrim prim -> (Map.empty, tiPrim prim)
    | EAbs(n, e) ->
        let tv = newTyVar "a"
        let env1 = env |> TypeEnv.remove n
        let env2 =
            env1 |> Map.union (Map.singleton n (Scheme([], tv) ))
        let (s1, t1) = ti env2 e
        s1, TFun( Typ.apply s1 tv, t1)
    | EApp(e1, e2) ->
        let s1, t1 = ti env e1
        let s2, t2 = ti (TypeEnv.apply s1 env) e2
        let tv = newTyVar "a"
        let s3 = unify (Typ.apply s2 t1) (TFun(t2, tv))
        Subst.compose (Subst.compose s3 s2) s1, Typ.apply s3 tv
    | ELet(x, e1, e2) ->
        let s1, t1 = ti env e1
        let env1 = env |> TypeEnv.remove x
        let scheme = generalize (TypeEnv.apply s1 env) t1
        let env2  =  env1 |> Map.add x scheme
        let s2, t2 = ti (env2 |> TypeEnv.apply s1 ) e2
        Subst.compose s1 s2, t2

let tiPrim prim =
    match prim with
    | Int _ -> TInt
    | Bool _ -> TBool
    | Cond -> 
        let a = newTyVar "a"
        TFun(TBool, TFun(a, TFun(a, a)))
    | RecordEmpty -> TRecord TRowEmpty
    | RecordSelect label -> 
        let a = newTyVar "a"
        let r = newTyVar "r"
        TFun (TRecord (TRowExtend(label, a, r)) , a)
    | RecordExtend label  ->
        let a = newTyVar "a"
        let r = newTyVar "r"
        TFun(a, TFun(TRecord r, TRecord(TRowExtend(label, a, r) )))
    | RecordRestrict label ->
        let a = newTyVar "a"
        let r = newTyVar "r"
        TFun(TRecord( (TRowExtend(label, a, r))), TRecord r)



//let typeInference //:: M.Map String Scheme -> Exp -> TI Type
let typeInference env e =
  let s, t = ti env e
  Typ.apply s t

let test1 =
    EApp (EApp(EPrim(RecordExtend "y"), (EPrim ( Int 2))), (EPrim RecordEmpty))

let test2 =
    EApp (EApp (EPrim(RecordExtend "x"), (EPrim (Int 1))), test1)

let test3 =
    EApp (EPrim (RecordSelect "y"), test2)

///let f = fun r -> (_.x) r in f
///:: { x = a5 | a7 } -> a6
///   { x = a4 | r5 } -> a4
let test4 =
    ELet("f", EAbs("r", EApp( EPrim(RecordSelect "x"), EVar "r")), EVar "f" )

///fun r -> (_.x) r
let test5 =
    EAbs("r", EApp( EPrim(RecordSelect "x") , EVar "r") )
