(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

open Type
open Exception

(*****************************************************************************)
(* Simplification d'un fichier .ml en Syntaxe Abstraite Polish *)

(*****************************************************************************)
(* Lecture et simplification *)

(** Simplifie une division *)
let simpl_division (exp1:expr) (exp2:expr) (ope:op) : expr =
    let bool = ope = Div in
    if isNum exp1 && isNum exp2 then
        if bool then Num(getNum exp1 / getNum exp2)
        else Num(getNum exp1 mod getNum exp2)
    else if exp1 = Num(0) || exp2 = Num(0) then Num(0)
    else if exp2 = Num(1) then exp1
    else if bool then Op(Div, exp1, exp2) else Op(Mod, exp1, exp2)
;;

(** Simplifie une expression *)
let rec simpl_expr (exp:expr) (pos:int) : expr =
    match exp with
    | Var(mot) -> Var(mot)
    | Num(mot) -> Num(mot)
    | Op(operateur, exp1, exp2) ->
        let exp1 = simpl_expr exp1 pos in
        let exp2 = simpl_expr exp2 pos in
        match operateur with
        | Add -> if isNum exp1 && isNum exp2 then
                Num(getNum exp1 + getNum exp2)
            else if exp1 = Num(0) then exp2
            else if exp2 = Num(0) then exp1
            else Op(Add, exp1, exp2)
        | Sub -> if isNum exp1 && isNum exp2 then
                Num(getNum exp1 - getNum exp2)
            else if exp2 = Num(0) then exp1 else Op(Sub, exp1, exp2)
        | Mul -> if isNum exp1 && isNum exp2 then
                Num(getNum exp1 * getNum exp2)
            else if (exp1 = Num(0) || exp2 = Num(0)) then Num(0)
            else if exp1 = Num(1) then exp2
            else if exp2 = Num(1) then exp1
            else Op(Mul, exp1, exp2)
        | Div -> simpl_division exp1 exp2 Div
        | Mod -> simpl_division exp1 exp2 Mod
;;

(** Simplifie une condition *)
let simpl_cond (exp1:expr) (exp2:expr) (comparateur:comp)
(pos:int) : cond =
    let exp1 = simpl_expr exp1 pos in
    let exp2 = simpl_expr exp2 pos in
    (exp1, comparateur, exp2)
;;

(** Donne le résultat d'une condition *)
let res_cond (exp1:expr) (comp:comp) (exp2:expr)
(pos:int) : bool =
    let x = getNum exp1 in
    let y = getNum exp2 in
    eval_condition_bool x y comp
;;

(** Simplifie un block en Syntaxe Abstraite Polish *)
let rec simpl_bl (block:block) : block =
    match block with
    | [] -> []
    | (pos, ligne_instruction)::suite ->
        match ligne_instruction with
        | Set(name, exp) -> let inst = Set(name, simpl_expr exp pos) in
            (pos, inst) :: simpl_bl suite
        | Read(name) -> (pos, Read(name)) :: simpl_bl suite
        | Print(exp) -> (pos, Print(simpl_expr exp pos)) :: simpl_bl suite
        | If((exp1, comp, exp2), exp3, exp4) ->
            simpl_if exp1 exp2 comp exp3 exp4 pos suite
        | While((exp1, comp, exp2), exp3) ->
            let (exp1, comp, exp2)  = simpl_cond exp1 exp2 comp pos in
            let inst = While((exp1, comp, exp2), simpl_bl exp3) in
            if (isNum exp1 && isNum exp2) then
                let (res_bool:bool) = res_cond exp1 comp exp2 pos in
                if res_bool then (pos, inst) :: simpl_bl suite
                else simpl_bl suite
            else (pos, inst) :: simpl_bl suite

and simpl_if (exp1:expr) (exp2:expr) (comp:comp) (exp3:block) (exp4:block)
(pos:int) (suite:block) =
    let (exp1, comp, exp2) = simpl_cond exp1 exp2 comp pos in
    if (isNum exp1 && isNum exp2) then
        let (res_bool:bool) = res_cond exp1 comp exp2 pos in
        if res_bool then simpl_bl exp3 @ simpl_bl suite
        else if exp4 <> [] then simpl_bl exp4 @ simpl_bl suite
        else simpl_bl suite
    else let inst = If((exp1, comp, exp2), simpl_bl exp3, simpl_bl exp4) in
        (pos, inst) :: simpl_bl suite

(*****************************************************************************)
(* Fonction terminale *)

(** Simplifie un Polish en Syntaxe Abstraite Polish *)
let simpl_polish (p:program) : program =
    simpl_bl p
;;

(*****************************************************************************)