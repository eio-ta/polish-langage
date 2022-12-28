(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

open Type

exception Erreur_Read of string
exception Erreur_Eval of string

(*****************************************************************************)
(* Mise en place des erreurs et des exceptions *)

(** Affiche une erreur lors de la lecture d'un fichier Polish *)
let error_read (str:string) (i:int) =
    raise (Erreur_Read ("Ligne " ^ string_of_int i ^ " : " ^ str))
;;

(** Affiche une erreur lors de l'évaluation d'un fichier Polish *)
let error_eval (str:string) (i:int) =
    raise (Erreur_Eval ("Ligne " ^ string_of_int i ^ " : " ^ str))
;;

(*****************************************************************************)