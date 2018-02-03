module rec Ast3
open System
open System.Collections.Generic
open Persistent

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
    
    let applySubst subst typ =
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

module Scheme =
   let freeTypeVariables (scheme: Scheme) =
       match scheme with
       | Scheme(variables, typ) ->
           let freeTypeVariables = Typ.freeTypeVariables typ
           freeTypeVariables - (Set.ofList variables)


   let applySubst (subst: Subst) (scheme: Scheme) =
       match scheme with
       | Scheme(variables, typ) ->
           let newSubst =
               variables
               |> List.fold (fun ns key -> ns |> Map.remove key) subst 
           let newTyp = typ |> Typ.applySubst newSubst
           Scheme(variables, newTyp)

module TypeEnv =
     let remove (var : string) (typEnv: TypeEnv)=
        typEnv.Remove var
    
     let freeTypeVariables (typEnv: TypeEnv) =
        typEnv
        |> Seq.fold (fun state (KeyValue(_key ,value)) ->
               Set.union state (value |> Scheme.freeTypeVariables)) Set.empty

     let apply (subst : Subst) (typEnv: TypeEnv) =
        typEnv
        |> Map.map (fun _k v -> v |> Scheme.applySubst subst)

module Subst =
    /// Apply `s1` to `s2` then merge the results
    let compose s1 s2 =
        let newSubst =
            s2 |> Map.map (fun _key value -> value |> Typ.applySubst s1)
        Map.union newSubst s1

///generalize abstracts a type over all type variables which are free
/// in the type but not free in the given type environment.
let generalize (env : TypeEnv) (typ : Typ) =
    let result = Typ.freeTypeVariables typ - TypeEnv.freeTypeVariables env
    let variables = Set.toList result
    Scheme(variables, typ)

let newTyVar =
    let nextIndex = ref 1
    fun n ->
        let nn = sprintf "%s%d" n !nextIndex
        nextIndex := !nextIndex + 1
        TVar nn

/// The instantiation function replaces all bound type variables in a
/// type scheme with fresh type variables.
let instantiate (ts : Scheme) =
    match ts with
    | Scheme(variables, typ) ->
        let nvars = variables |> List.map (fun _ -> newTyVar "a") 
        let s = Map.ofSeq (Seq.zip variables nvars)
        typ |> Typ.applySubst s

let test1 =
    TFun(TVar "a", TVar "a")
