(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

open Type
open Exception

(*****************************************************************************)
(* Evaluation d'un fichier .ml en Syntaxe Abstraite Polish *)

(*****************************************************************************)
(* Lecture et évaluation *)

(** Evalue une expression en Syntaxe Abstraite Polish *)
let rec eval_expression (exp:expr) (env:int Environnement.t)
(pos:int) : int =
    match exp with
    | Var(mot) -> let option = Environnement.find_opt mot env in
        getValue option
    | Num(mot) -> mot
    | Op(operateur, exp1, exp2) ->
        let exp1 = eval_expression exp1 env pos in
        let exp2 = eval_expression exp2 env pos in
        match operateur with
        | Add -> exp1 + exp2
        | Sub -> exp1 - exp2
        | Mul -> exp1 * exp2
        | Div -> if exp2 <> 0 then exp1 / exp2
            else error_eval "Division par zéro" pos
        | Mod -> if exp2 <> 0 then exp1 mod exp2
            else error_eval "Division par zéro" pos
;;

(** Evalue un changement de variable en Syntaxe Abstraite Polish *)
let eval_set (name:string) (exp:expr) (env:int Environnement.t)
(pos:int) : int Environnement.t =
    let resultat = eval_expression exp env pos in
    if Environnement.find_opt name env = None then
        Environnement.add name resultat env
    else
        let env = Environnement.remove name env in
        Environnement.add name resultat env
;;

(** Evalue une condition en Syntaxe Abstraite Polish *)
let eval_condition (exp1:expr) (exp2:expr) (operateur:comp)
(env:int Environnement.t) (pos:int) : bool =
    let exp1 = eval_expression exp1 env pos in
    let exp2 = eval_expression exp2 env pos in
    eval_condition_bool exp1 exp2 operateur
;;

(** Evalue un block en Syntaxe Abstraite Polish *)
let rec eval_block (block:block) (env:int Environnement.t) :
int Environnement.t =
    match block with
    | [] -> env
    | (pos, ligne_instruction)::suite ->
        match ligne_instruction with
        | Set(name, exp) ->
            let env = eval_set name exp env pos in
            eval_block suite env
        | Read(name) ->
            print_string (name ^ "?");
            let nombre_choisi = read_int() in
            let env = Environnement.add name nombre_choisi env in
            eval_block suite env
        | Print(exp) ->
            let (resultat:int) = eval_expression exp env pos in
            print_int resultat;
            print_newline();
            eval_block suite env
        | If((exp1, comp, exp2), exp3, exp4) when exp4 <> [] ->
            let (cond:bool) = eval_condition exp1 exp2 comp env pos in
            let env = eval_block_if_1 exp3 exp4 cond env in
            eval_block suite env
        | If((exp1, comp, exp2), exp3, _) ->
            let (cond:bool) = eval_condition exp1 exp2 comp env pos in
            let env = eval_block_if_2 exp3 cond env in
            eval_block suite env
        | While((exp1, comp, exp2), exp3) ->
            let env = eval_block_while exp1 exp2 comp exp3 env pos in
            eval_block suite env

(** Evalue un block IF en Syntaxe Abstraite Polish *)
and eval_block_if_1 (exp3:block) (exp4:block) (cond:bool)
(env:int Environnement.t) : int Environnement.t =
    if cond then eval_block exp3 env else eval_block exp4 env

(** Evalue un block ELSE en Syntaxe Abstraite Polish *)
and eval_block_if_2 (exp3:block) (cond:bool)
(env:int Environnement.t) : int Environnement.t =
    if cond then eval_block exp3 env else env

(** Evalue un block WHILE en Syntaxe Abstraite Polish *)
and eval_block_while (exp1:expr) (exp2:expr) (comp:comp)
(exp3:block) (env:int Environnement.t)
(pos:int) : int Environnement.t =
    let rec eval_block_while_aux exp1 exp2 comp exp3 env =
        if eval_condition exp1 exp2 comp env pos then
            eval_block_while_aux exp1 exp2 comp exp3 (eval_block exp3 env)
        else env
    in eval_block_while_aux exp1 exp2 comp exp3 env
;;

(*****************************************************************************)
(* Fonction terminale *)

(** Evalue un fichier en Polish en Syntaxe Abstraite Polish *)
let eval_polish (p:program) : unit =
    let env = eval_block p (Environnement.empty) in
    if Environnement.is_empty env then print_newline();
;;

(*****************************************************************************)