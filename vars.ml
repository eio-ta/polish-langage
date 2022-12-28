(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

open Type

(*****************************************************************************)
(* Repère les variables d'un fichier .ml en Syntaxe Abstraite Polish *)

(*****************************************************************************)
(* Fonctions auxilaires *)

(** Vérifie si le sous-mot mot2 est contenu dans le mot mot1 *)
let contains (mot1:string) (mot2:string) : bool =
    try let len = String.length mot2 in
        for i = 0 to String.length mot1 - len do
            if String.sub mot1 i len = mot2 then raise Exit
        done; false
    with Exit -> true
;;

(** Modifie le nom d'une variable mal initialisée *)
let change_name (vars:Names.t) : Names.t =
    Names.map (fun mot -> if contains mot " INIT" then
    String.sub mot 0 (String.length mot - 5) else mot) vars
;;

(** Obtiens le tableau des variables initialisées *)
let get_init_vars (vars:Names.t) : Names.t =
    let filter str = not (contains str " INIT") in
    Names.filter filter vars
;;

(** Ajoute une variable dans la table de mémoire *)
let add_vars (mot:string) (vars:Names.t) (init:bool) : Names.t =
    if not init then
        if Names.mem (mot ^ " INIT") vars then vars
        else Names.add mot vars
    else if Names.mem mot vars then vars
        else Names.add (mot ^ " INIT") vars
;;

(** Supprime les variables entrées plusieurs fois *)
let rec remove_vars (vars:Names.t) (to_remove:Names.t) : Names.t =
    let mot = Names.choose_opt to_remove in
    if mot <> None then
        let vars = Names.remove ((getValue mot) ^ " INIT") vars in
        let to_remove = Names.remove (getValue mot) to_remove in
        remove_vars vars to_remove
    else vars
;;

(** Affiche toutes les variables *)
let print_set_all_vars (vars:Names.t) =
    Names.iter (fun mot ->
        let bool = contains mot " INIT" in
        if bool then
            let sub = String.sub mot 0 (String.length mot - 5) ^ " " in
            print_string sub;
        if not bool then print_string (mot ^ " ");
        ) vars
;;

(** Affiche toutes les variables non initialisées *)
let print_set_vars_non_init (vars:Names.t) =
    Names.iter (fun mot ->
        let bool = contains mot " INIT" in
        if not bool then print_string (mot ^ " ");
        ) vars
;;

(*****************************************************************************)
(* Lecture et repère des variables *)

(** Repère les variables dans les expressions *)
let rec vars_expression (exp:expr) (vars:Names.t) : Names.t =
    match exp with
    | Var(mot) -> add_vars mot vars false
    | Num(mot) -> vars
    | Op(operateur, exp1, exp2) -> vars_union exp1 exp2 vars

(** Fait l'union de deux tableaux de mémoire *)
and vars_union (exp1:expr) (exp2:expr) (vars:Names.t) : Names.t =
    let names_1 = vars_expression exp1 vars in
    let names_2 = vars_expression exp2 vars in
    Names.union names_1 names_2
;;

(** Repère les variables dans une ligne SET *)
let vars_set (name:string) (exp:expr) (vars:Names.t) : Names.t =
    let vars = add_vars name vars true in
    vars_expression exp vars
;;

(** Repère les variables dans un block *)
let rec vars_block (block:block) (vars:Names.t) : Names.t =
    match block with
    | [] -> vars
    | (position, ligne_instruction)::suite ->
        match ligne_instruction with
        | Set(name, exp) -> vars_block suite (vars_set name exp vars)
        | Read(name) -> vars_block suite (add_vars name vars true)
        | Print(exp) -> vars_block suite (vars_expression exp vars)
        | If((exp1, comp, exp2), exp3, exp4) when exp4 <> [] ->
            (*On gère le cas IF-ELSE *)
            let vars_res = vars_block_if_else exp1 comp exp2 exp3 exp4 vars in

            (* On lit la suite *)
            vars_block suite vars_res
        | If((exp1, comp, exp2), exp3, _) ->
            let vars_avant = vars_union exp1 exp2 vars in

            (* On lit les variables dans le block IF et on compare avec la
            mémoire du block d'avant *)
            let vars_block_if = vars_block exp3 vars_avant in
            let vars_diff = Names.diff vars_block_if vars_avant in

            (* On lit la suite *)
            vars_block suite (Names.union vars_avant (change_name vars_diff))
        | While((exp1, comp, exp2), exp3) ->
            let vars_avant = vars_union exp1 exp2 vars in

            (* On lit les variables dans le block WHILE et on compare avec
            la mémoire du block d'avant *)
            let vars_block_while = vars_block exp3 vars_avant in
            let vars_diff = Names.diff vars_block_while vars_avant in

            (* On lit la suite *)
            vars_block suite (Names.union vars_avant (change_name vars_diff))

(** Repère les variables dans un block IF et ELSE *)
and vars_block_if_else (exp1:expr) (comp:comp) (exp2:expr) (exp3:block)
(exp4:block) (vars:Names.t) : Names.t =
    let vars_avant = vars_union exp1 exp2 vars in

    (* On lit les variables dans les blocks IF et ELSE *)
    let vars_block_if = vars_block exp3 vars_avant in
    let vars_block_else = vars_block exp4 vars_avant in
    let vars_union = Names.union vars_block_if vars_block_else in

    (* On repère les différences et on supprimer les variables si elles
    sont mal initialisées *)
    let vars_res = change_name (get_init_vars vars_union) in
    remove_vars vars_union vars_res
;;

(*****************************************************************************)
(* Fonction terminale *)

(** Repère les variables initialisées et non initialisées *)
let vars_polish (p:program) : unit =
    let vars = vars_block p Names.empty in
    print_set_all_vars vars;
    print_newline ();
    print_set_vars_non_init vars;
    print_newline ();
;;

(*****************************************************************************)