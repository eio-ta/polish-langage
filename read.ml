(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

open Type
open Exception

(*****************************************************************************)
(* Traduction d'un fichier .ml en Syntaxe Abstraite Polish *)

(*****************************************************************************)
(* Fonctions auxiliaires qui touche les modules classiques *)

(** Vérifie si un string est dans une liste de string*)
let contains_in_a_list (elt:string) (ligne:string list) : bool =
    List.find_opt (fun mot -> mot = elt) ligne <> None
;;

(*****************************************************************************)
(* Fonctions auxiliaires pour la lecture du programme *)

(** Fonction permettant de connaître la nature d'un mot *)
let nature_of_string (mot:string) : string =
    try int_of_string mot |> ignore; "number"
    with Failure _ ->
        if contains_in_a_list mot ["+"; "-"; "*"; "/"; "%"] then "operator"
        else let conditions = ["="; "<>"; "<"; "<="; ">"; ">="; ":="] in
        if contains_in_a_list mot conditions then
        "condition" else "variable"
;;

(** Compte le nombre d'espace en début de ligne *)
let count_first_space (ligne:string) : int =
    let taille = String.length ligne in
    let rec count_first_space_aux indice (ligne:string) : int =
        if (indice < taille) then
        if ligne.[indice] <> ' ' then indice 
        else count_first_space_aux (indice+1) ligne
        else -1
    in count_first_space_aux 0 ligne
;;

(** Supprime les mots vides dans une liste de mots *)
let rec remove_empty_words_in_list (mots:string list) : string list =
    match mots with
    | [] -> []
    | elt::suite -> if elt = "" then remove_empty_words_in_list suite
    else elt::remove_empty_words_in_list suite
;;

(** Renvoie le nombre d'espace en début de ligne et la ligne correspondante *)
let get_number_space_and_line (ligne:string) : (int * string list) =
    let espace = count_first_space ligne in
    (espace, remove_empty_words_in_list (String.split_on_char ' ' ligne))
;;

(** Vérifie s'il existe un ELSE dans la liste des lignes *)
let is_a_block_else (lignes:(int*string) list) =
    match lignes with
    | [] -> false
    | (elt, ligne)::_ -> let liste = String.split_on_char ' ' ligne in
        let liste = remove_empty_words_in_list liste in
        if (List.hd liste) = "ELSE" then true else false
;;

(*****************************************************************************)
(* Lecture et traduction *)

(** Traduis un fichier .ml en (int * string) list *)
let read_file (nom_du_fichier:string) : (int * string) list =
    let fichier = open_in nom_du_fichier in
    let rec read ligne pos = 
        try let l = input_line ligne in
            (pos, l) :: (read fichier (pos + 1))
        with End_of_file -> []
    in read fichier 1
;;

(** Traduis une expression en Polish avec une Syntaxe Abstraite Polish *)
let rec read_expr (mots:string list)
(pos:int) : expr * (string list) =
    match mots with
    | [] -> error_read "Pas d'expression (EXPRESSION)" pos
    | elt::suite -> let nature_elt = nature_of_string elt in
        if nature_elt = "number" then (Num(int_of_string elt), suite)
        else if nature_elt = "variable" then (Var(elt), suite)
        else if nature_elt = "operator" then
        let (exp1, suite_2) = read_expr suite pos in
        let (exp2, fin_de_mots) = read_expr suite_2 pos in
        match elt with
        | "+" -> (Op(Add, exp1, exp2), fin_de_mots)
        | "-" -> (Op(Sub, exp1, exp2), fin_de_mots)
        | "*" -> (Op(Mul, exp1, exp2), fin_de_mots)
        | "/" -> (Op(Div, exp1, exp2), fin_de_mots)
        | "%" -> (Op(Mod, exp1, exp2), fin_de_mots)
        | _ -> error_read "Opérateur non reconnu (EXPRESSION)" pos
        else error_read "Expression non reconnue (EXPRESSION)" pos
;;

(** Traduis une expression en Polish avec une Syntaxe Abstraite Polish *)
let read_cond (mots:string list) (pos:int) : cond =
    let read_cond_aux (mots:string list) (exp1:expr)
    (comparateur:comp) : cond =
        let (exp2, suite) = read_expr mots pos in
        match suite with
        | [] -> (exp1, comparateur, exp2)
        | _ -> error_read "Condition non reconnue (CONDITION)" pos
    in let (exp1, suite) = read_expr mots pos in
    match suite with
    | [] -> error_read "Expression non reconnue (CONDITION)" pos
    | elt::exp2 -> match elt with
        | "=" -> read_cond_aux exp2 exp1 Eq
        | "<" -> read_cond_aux exp2 exp1 Lt
        | ">" -> read_cond_aux exp2 exp1 Gt
        | "<>" -> read_cond_aux exp2 exp1 Ne
        | "<=" -> read_cond_aux exp2 exp1 Le
        | ">=" -> read_cond_aux exp2 exp1 Ge
        | _ -> error_read "Opérateur non reconnu (CONDITION)" pos
;;

(** Traduis un changement de variable en Polish en Syntaxe Abstraite Polish *)
let read_set (mots:string list) (pos:int) : instr =
    match mots with
    | [] -> error_read "Pas de changement de variable (SET)" pos
    | elt::suite -> let nature_elt = nature_of_string elt in
        if nature_elt <> "variable" then
            error_read "Nom de variable invalide (SET)" pos
        else match suite with
        | [] -> error_read "Expression non reconnue (SET)" pos
        | elt2 :: suite_suite -> if elt2 <> ":=" then
                error_read "PAS DE \":=\" (SET)" pos
            else let (exp, fin_de_liste) = read_expr suite_suite pos in
            if fin_de_liste <> [] then
                error_read "Beaucoup trop d'arguments (SET)" pos
            else Set(elt, exp)
;;

(** Traduis une nouvelle variable en Polish en Syntaxe Abstraite Polish *)
let read_read (mots:string list) (pos:int) : instr =
    match mots with
    | [] -> error_read "Pas de variable (READ)" pos
    | elt :: fin_de_liste -> if fin_de_liste <> [] then
            error_read "Beaucoup trop d'arguments (READ)" pos
        else let nature_elt = nature_of_string elt in
        if nature_elt = "variable" then Read(elt)
        else error_read "Mauvais nom de variable (READ)" pos
;;

(** Traduis une ligne à afficher en Polish en Syntaxe Abstraite Polish *)
let read_print (mots:string list) (pos:int) : instr = 
    match mots with
    | [] -> error_read "Pas de variable / d'expression (PRINT)" pos
    | elt -> let (exp, suite) = read_expr elt pos in
        if suite <> [] then
            error_read "Beaucoup trop d'arguments (PRINT)" pos
        else Print(exp)
;;

(** Traduis une liste de ligne en block de la Syntaxe Abstraite Polish *)
let rec read_block (ind_voulue:int)
(lignes:(int * string) list)
(have_a_block_else:bool) : block * ((int * string) list) =
    match lignes with
    | [] -> ([],[])
    | (pos, ligne_string)::suite ->
        let (nbr_espace, ligne) = get_number_space_and_line ligne_string in
        if nbr_espace < ind_voulue then
            if (nbr_espace mod 2) = 0 then ([], lignes)
            else error_read "Indentation de longueur impaire" pos
        else if nbr_espace > ind_voulue then
                error_read "Erreur d'indentation" pos
            else if have_a_block_else then
                read_else ind_voulue ligne lignes pos
            else let (inst, suite_2)= read_instr nbr_espace ligne suite pos in
            let (block_1, suite_3) = read_block nbr_espace suite_2 false in
            if inst = None then (block_1, suite_3)
            else ((pos, Option.get inst)::block_1, suite_3)


(** Traduis une liste de ligne en block (contenant un ELSE) de la
Syntaxe Abstraite Polish *)
and read_else (ind_voulue:int) (mots:string list)
(lignes:(int * string) list)
(pos:int) : block * ((int *string) list) =
    match mots with
    | [] -> error_read "Mauvaise écriture du block (ELSE)" pos
    | elt::suite -> if(elt = "ELSE" && suite = []) then
        read_block (ind_voulue + 2) (List.tl lignes) false
        else read_block ind_voulue lignes false


(** Traduis une instruction en block de la Syntaxe Abstraite Polish *)
and read_instr (ind_voulue:int) (mots:string list)
(suite:(int*string) list) (pos:int) : 'instr option * (int * string) list =
    match mots with
    | [] -> error_read "Mauvaise écriture de l'instruction" pos
    |elt::suite_2 -> match elt with
        | "COMMENT" -> (None, suite)
        | "READ" -> (Some (read_read suite_2 pos), suite)
        | "PRINT" -> (Some (read_print suite_2 pos), suite)
        | "ELSE" -> error_read "Bloc \"Else\" avant un \"If\"" pos
        | "WHILE" ->
            let (wh_instr, suite_3) = read_wh ind_voulue suite_2 suite pos in
            (Some(wh_instr),suite_3)
        | "IF" ->
            let (if_instr, suite_3) = read_if ind_voulue suite_2 suite pos in
            (Some(if_instr), suite_3)
        | _ -> try (Some (read_set mots pos), suite)
            with Failure (s) -> error_read "Mauvaise écriture du \"If\"" pos

(** Traduis une liste de ligne en block (contenant un WHILE) de la
Syntaxe Abstraite Polish *)
and read_wh (ind_voulue:int) (mots:string list)
(lignes:(int *string) list) (pos:int) : instr * ((int * string) list) =
    let condition = read_cond mots pos in
    let block, suite = read_block (ind_voulue + 2) lignes false in
    (While(condition, block), suite)

(** Traduis une liste de ligne en block (contenant un IF) de la
Syntaxe Abstraite Polish *)
and read_if (ind_voulue: int) (mots: string list)
(lignes: (int * string) list) (pos:int) : instr * ((int *string) list) =
    let condition = read_cond mots pos in
    let block_1, suite = read_block (ind_voulue + 2) lignes false in
    let block2, suite_2 = read_block (ind_voulue + 2)
    (if suite <> [] then
        if is_a_block_else suite then List.tl suite else suite
    else []) true
    in (If(condition, block_1, block2), suite_2)
;;

(*****************************************************************************)
(* Fonction terminale *)

(** Traduis un fichier en Polish en Syntaxe Abstraite Polish *)
let read_polish (filename:string) : program =
    let lignes = read_file filename in
    let (block, list) = read_block 0 lignes false in
    block
;;

(*****************************************************************************)