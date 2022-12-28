(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

open Type

(*****************************************************************************)
(* Evaluation des signes possibles des variables d'un fichier .ml en Syntaxe
Abstraite Polish *)

let is_safe = ref "safe"

(*****************************************************************************)
(* Fonctions auxilaires, touchant les modules classiques *)

(** Ajoute une variable à l'environnement *)
let add_signs (name:string) (signs:sign list)
(env:sign list Environnement.t) : sign list Environnement.t =
    if Environnement.find_opt name env = None then
        Environnement.add name signs env
    else let env = Environnement.remove name env in
        Environnement.add name signs env
;;

(** Retourne l'union entre deux environnements *)
let union_env (environ_1:sign list Environnement.t)
(environ_2:sign list Environnement.t) : sign list Environnement.t =
    let rec union_environ_aux (signs:sign list)
    (res_1:sign list) (res_2:sign list) : sign list =
        match signs with
        | [] -> []
        | sign::suite ->
            let contains_sign_1 = List.mem sign res_1 in
            let contains_sign_2 = List.mem sign res_2 in
            if contains_sign_1 || contains_sign_2 then
                [sign] @ union_environ_aux suite res_1 res_2
            else union_environ_aux suite res_1 res_2
    in Environnement.merge (fun f option_1 option_2 ->
        match option_1, option_2 with
        | Some x, Some y ->
            Some (union_environ_aux [Neg; Zero; Pos; Error] x y)
        | None, option_2 -> option_2
        | option_1, None -> option_1
    ) environ_1 environ_2
;;

(** Change le message d'erreur *)
let change_error (pos:int) : unit =
    if is_safe <> ref "safe" then
    let res = "divbyzero " ^ string_of_int pos in
    is_safe := res;
    print_string !is_safe;
;;

(*****************************************************************************)
(* Fonctions auxilaires, touchant les nombres et les expressions *)

(** Donne le signe d'un chiffre *)
let recognize_sign_number (number:int) : sign =
    if number < 0 then Neg
    else if number = 0 then Zero
    else Pos
;;

(** Inverse les signes *)
let rec sign_inverse (signs:sign list) : sign list =
    match signs with
    | [] -> []
    | Neg::suite -> [Pos] @ sign_inverse suite
    | Pos::suite -> [Neg] @ sign_inverse suite
    | sign::suite -> [sign] @ sign_inverse suite
;;

(*****************************************************************************)
(* Fonctions auxilaires, touchant les signes des variables *)

(** Affiche les signes *)
let rec sign_and_string (signs:sign list) : string =
    match signs with
    | [] -> ""
    | Neg::suite -> "-" ^ sign_and_string suite
    | Zero::suite -> "0" ^ sign_and_string suite
    | Pos::suite -> "+" ^ sign_and_string suite
    | Error::suite -> "!" ^ sign_and_string suite
;;

(** Affiche les signes *)
let print_signs (environ:sign list Environnement.t) =
    let print_sign_aux key value =
        print_string (key ^ " " ^ (sign_and_string value) ^ "\n")
    in Environnement.iter print_sign_aux environ
;;

(*****************************************************************************)
(* Lecture et évaluation des signes *)

(** Trouve les signes d'une expression *)
let rec sign_expr (exp:expr) (pos:int)
(env:sign list Environnement.t) : sign list =
    match exp with
    | Var(mot) -> let signs = getValue (Environnement.find_opt mot env) in
        signs
    | Num(mot) -> [recognize_sign_number mot]
    | Op(operateur, exp1, exp2) ->
        let sign1 = sign_expr exp1 pos env in
        let sign2 = sign_expr exp2 pos env in
        let be_0_sign1 = List.mem Zero sign1 in
        let be_0_sign2 = List.mem Zero sign2 in
        match operateur with
        | Add -> if sign1 = sign2 then sign1 else [Neg; Zero; Pos]
        | Sub -> let sign2 = sign_inverse sign2 in
            if sign1 = sign2 then sign1 else [Neg; Zero; Pos]
        | Mul -> if be_0_sign1 || be_0_sign2 then
                if sign1 = sign2 then [Zero; Pos] else [Neg; Zero]
            else if sign1 = sign2 then [Pos] else [Neg]
        | Div -> sign_expr_div_mod sign1 sign2 be_0_sign1 be_0_sign2 pos
        | Mod -> sign_expr_div_mod sign1 sign2 be_0_sign1 be_0_sign2 pos

(** Trouve les signes d'une division ou d'un modulo *)
and sign_expr_div_mod (sign1:sign list) (sign2:sign list) (be_0_sign1:bool)
(be_0_sign2:bool) (pos:int) : sign list =
    if be_0_sign2 then
        if is_safe = ref "safe" then
            is_safe := "divbyzero " ^ string_of_int pos;
    if be_0_sign2 then
        if sign1 = sign2 then [Zero; Pos; Error] else [Neg; Zero; Error]
    else if be_0_sign1 then
        if sign1 = sign2 then [Zero; Pos] else [Neg; Zero]
    else if sign1 = sign2 then [Pos] else [Neg]
;;

(** Ajoute une variable à l'environnement (SET et READ) *)
let sign_set (name:string) (exp:expr) (pos:int)
(env:sign list Environnement.t) : sign list Environnement.t =
    let signs = sign_expr exp pos env in
    add_signs name signs env
;;

(** Change les signes d'une variable utilisée dans une condition *)
let get_sign_cond (exp1:expr) (sign1:sign list) (sign2:sign list)
(comp:comp) (env:sign list Environnement.t) : sign list Environnement.t =
    let name = getVar exp1 in
    match comp with
    | Eq -> add_signs name sign2 env
    | Ne -> if sign2 = [Zero] then add_signs name [Neg; Pos] env
        else add_signs name [Neg; Zero; Pos] env
    | Lt -> if sign2 = [Zero] || sign2 = [Neg; Zero] || sign2 = [Neg]
        then add_signs name [Neg] env
        else add_signs name [Neg; Zero; Pos] env
    | Le -> if sign2 = [Zero] || sign2 = [Neg; Zero] || sign2 = [Neg]
        then add_signs name [Neg; Zero] env
        else add_signs name [Neg; Zero; Pos] env
    | Gt -> if sign2 = [Zero] || sign2 = [Neg; Zero] || sign2 = [Neg]
        then add_signs name [Pos] env
        else add_signs name [Neg; Zero; Pos] env
    | Ge -> if sign2 = [Zero] || sign2 = [Neg; Zero] || sign2 = [Neg]
        then add_signs name [Zero; Pos] env
        else add_signs name [Neg; Zero; Pos] env
;;

(** Analyse d'une condition *)
let sign_cond (exp1:expr) (exp2:expr) (comp:comp) (pos:int)
(env:sign list Environnement.t) : sign list Environnement.t =
    let sign1 = sign_expr exp1 pos env in
    let sign2 = sign_expr exp2 pos env in
    let is_variable_sign1 = isVar exp1 in
    let is_variable_sign2 = isVar exp2 in
    if not is_variable_sign1 && not is_variable_sign2 then env
    else if is_variable_sign1 then
        get_sign_cond exp1 sign1 sign2 comp env
    else if is_variable_sign2 then
        get_sign_cond exp2 sign2 sign1 (get_comp_diff comp) env
    else env
;;

(** Change les signes d'une variable dans un block *)
let rec sign_block (block:block)
(env:sign list Environnement.t) : sign list Environnement.t =
    match block with
    | [] -> env
    | (pos, ligne_instruction)::suite ->
        match ligne_instruction with
        | Set(name, exp) -> sign_block suite (sign_set name exp pos env)
        | Read(name) -> sign_block suite (add_signs name [Neg; Zero; Pos] env)
        | Print(exp) -> sign_block suite env
        | If((exp1, comp, exp2), exp3, exp4) when exp4 <> [] ->
            let cond_if = sign_cond exp1 exp2 comp pos env in
            let cond_el = sign_cond exp1 exp2 (get_comp_diff comp) pos env in
            let env_if = sign_block exp3 cond_if in
            let env_el = sign_block exp4 cond_el in
            let env = union_env env_if env_el in
            sign_block suite env
        | If((exp1, comp, exp2), exp3, _) ->
            let env = sign_cond exp1 exp2 comp pos env in
            let env = sign_block exp3 env in
            sign_block suite env
        | While((exp1, comp, exp2), exp3) ->
            let cond = sign_cond exp1 exp2 comp pos env in
            let env_1 = sign_block exp3 cond in
            let env_2 = union_env env env_1 in
            let env_wh = sign_wh env exp1 pos exp2 comp exp3 env_2 in
            let env_c = sign_cond exp1 exp2 (get_comp_diff comp) pos env_wh in
            sign_block suite env_c

(** Change les signes d'une variable dans un block WHILE *)
and sign_wh (env_debut:sign list Environnement.t) (exp1:expr) (pos:int)
(exp2:expr) (comp:comp) (exp3:block)
(env_finale:sign list Environnement.t) : sign list Environnement.t =
    let sign_wh_aux res_1 res_2 = res_1 = res_2 in
    if Environnement.equal sign_wh_aux env_finale env_debut then env_debut
    else let cond = sign_cond exp1 exp2 comp pos env_finale in
        let env_1 = sign_block exp3 cond in
        let env_2 = union_env env_finale env_1 in
        sign_wh env_finale exp1 pos exp2 comp exp3 env_2
;;

(*****************************************************************************)
(* Fonction terminale *)

(** Evalue un fichier en Polish en Syntaxe Abstraite Polish *)
let sign_polish (p:program) : unit =
    let env = sign_block p Environnement.empty in
    print_signs env;
    print_string !is_safe;
    print_newline ();
;;

(*****************************************************************************)