(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

open Type

(*****************************************************************************)
(* Affichage d'un programme en Syntaxe Abstraite Polish *)

(** Affiche une expression en Syntaxe Abstraite Polish*)
let rec print_expression expression =
    match expression with
    | Var(mot) -> print_string mot;
    | Num (mot) -> print_int mot;
    | Op(operateur, exp1, exp2) -> match operateur with
        | Add -> print_expression_aux exp1 exp2 "+";
        | Sub -> print_expression_aux exp1 exp2 "-";
        | Mul -> print_expression_aux exp1 exp2 "*";
        | Div -> print_expression_aux exp1 exp2 "/";
        | Mod -> print_expression_aux exp1 exp2 "%";

and print_expression_aux (exp1:expr) (exp2:expr) (operateur:string) =
    print_string operateur; print_string " "; print_expression exp1;
    print_string " "; print_expression exp2;
;;

(** Affiche une condition en Syntaxe Abstraite Polish*)
let rec print_condition ((operateur, exp1, exp2):comp * expr * expr) =
    match operateur with
    | Eq -> print_condition_aux exp1 exp2 "=";
    | Ne -> print_condition_aux exp1 exp2 "<>";
    | Lt -> print_condition_aux exp1 exp2 "<";
    | Le -> print_condition_aux exp1 exp2 "<=";
    | Gt -> print_condition_aux exp1 exp2 ">";
    | Ge -> print_condition_aux exp1 exp2 ">=";

and print_condition_aux (exp1:expr) (exp2:expr) (operateur:string) =
    print_expression exp1;
    print_string (" " ^ operateur ^ " ");
    print_expression exp2;
;;

(** Affiche un changement de variable en Syntaxe Abstraite Polish *)
let print_set (name:string) (expression:expr) =
    print_string (name ^ " := ");
    print_expression expression;
    print_newline();
;;

(** Affiche un block en Syntaxe Abstraite Polish *)
let rec print_block (indentation_attendue:int) (block:block) =
    match block with
    | [] -> print_string "";
    | (position, ligne_instruction)::suite ->
        match ligne_instruction with
        | Set(name, expr) ->
            print_space indentation_attendue;
            print_set name expr;
            print_block indentation_attendue suite;
        | Read(name) ->
            print_space indentation_attendue;
            print_string ("READ " ^ name); print_newline();
            print_block indentation_attendue suite;
        | Print(expr) ->
            print_space indentation_attendue;
            print_string "PRINT "; print_expression expr; print_newline();
            print_block indentation_attendue suite;
        | If((exp1, comp, exp2), exp3, exp4) when exp4 <> [] ->
            print_space indentation_attendue;
            print_block_if exp1 comp exp2 exp3 exp4 indentation_attendue;
            print_block (indentation_attendue) suite;
        | If((exp1, comp, exp2), exp3, _) ->
            print_space indentation_attendue;
            print_string "IF "; print_condition (comp, exp1, exp2);
            print_newline(); print_block (indentation_attendue+2) exp3;
            print_block (indentation_attendue) suite;
        | While((exp1, comp, exp2),exp3) ->
            print_space indentation_attendue;
            print_string "WHILE "; print_condition (comp, exp1, exp2);
            print_newline(); print_block (indentation_attendue+2) exp3;
            print_block (indentation_attendue) suite;

(** Affiche un block IF en Syntaxe Abstraite Polish *)
and print_block_if (exp1:expr) (comp:comp) (exp2:expr) (exp3:block)
(exp4:block) (indentation_attendue:int) =
    print_string "IF "; print_condition (comp, exp1, exp2); print_newline();
    print_block (indentation_attendue+2) exp3;
    print_space indentation_attendue;
    print_string "ELSE"; print_newline();
    print_block (indentation_attendue+2) exp4;

(** Affiche un nombre X d'espace *)
and print_space (indentation:int) =
    print_string (String.init indentation (function x -> ' '));
;;

(*****************************************************************************)
(* Fonction terminale *)

(** Affiche un programme en Syntaxe Abstraite Polish *)
let print_polish (p:program) : unit =
    print_block 0 p;
;;

(*****************************************************************************)