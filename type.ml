(*****************************************************************************)
(* Syntaxe abstraite Polish *)

(*****************************************************************************)
(* Types initiaux *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
  | Eq (* = *)
  | Ne (* Not equal, <> *)
  | Lt (* Less than, < *)
  | Le (* Less or equal, <= *)
  | Gt (* Greater than, > *)
  | Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block

(*****************************************************************************)
(* Type et Module ajoutés *)

(** Environnement *)
module Environnement = Map.Make(String)

(** Liste des variables *)
module Names = Set.Make(String)

(** Signes des variables : - 0 + ! *)
type sign = Neg | Zero | Pos | Error

(*****************************************************************************)
(* Fonctions en lien direct avec les types *)

(* EXPRESSIONS *)

(** Vérifie si une expression est une variable *)
let isVar (exp:expr) : bool =
    match exp with
    | Var(mot) -> true
    | _ -> false
;;

(** Récupère le nom de la variable *)
let getVar (exp:expr) : string =
    let error = "Ce n'est pas une variable" in
    match exp with
    | Var(mot) -> mot
    | _ -> failwith error
;;

(** Vérifie si une expression est un nombre *)
let isNum (exp:expr) : bool =
    match exp with
    | Num(mot) -> true
    | _ -> false

(** Récupère le nombre de l'expression *)
let getNum (exp:expr) : int =
    let error = "Ce n'est pas un nombre" in
    match exp with
    | Num(mot) -> mot
    | _ -> failwith error

(* COMPARAISON - CONDITION *)

(** Retourne la comparaison inverse de la comparaison donnée en argument *)
let get_comp_diff (comp:comp) : comp =
    match comp with
    | Eq -> Ne
    | Ne -> Eq
    | Lt -> Ge
    | Le -> Gt
    | Gt -> Le
    | Ge -> Lt
;;

(** Evalue le résultat d'une condition pour la simplification d'un block *)
let eval_condition_bool (x:int) (y:int) (operateur:comp) : bool =
    match operateur with
    | Eq -> x = y
    | Ne -> x <> y
    | Lt -> x < y
    | Le -> x <= y
    | Gt -> x > y
    | Ge -> x >= y
;;

(** OPTIONS *)

(** Récupère la vleur d'une option *)
let getValue (value:'a option): 'a =
    let error = "Variable non définie" in
    match value with
    | None -> failwith error
    | Some(x) -> x
;;

(*****************************************************************************)