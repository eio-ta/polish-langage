(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

open Type
open Read
open Reprint
open Eval
open Simpl
open Vars
open Sign

(*****************************************************************************)
(** Lancement du programme *)

let usage () =
  print_string "Projet Polish -- Analyse statique d'un mini-langage ";
  print_string "impératif\n\n Si vous voyez ces lignes, c'est que vous avez ";
  print_string "sans doute renseigné une mauvaise information. Il faut deux ";
  print_string "arguments après la commande de lancement.\n\n Pour le premier,";
  print_string " vous pouvez mettre :\n";
  print_string "   \"-reprint\" : pour afficher le programme Polish\n";
  print_string "   \"-eval\" : pour évaluer le programme Polish\n";
  print_string "   \"-simpl\" : pour simplifier le programme Polish et ";
  print_string "l'afficher\n";
  print_string "   \"-vars\" : pour avoir les variables initialisées et ";
  print_string "non-initialisées\n";
  print_string "   \"-sign\" : pour avoir les signes possibles des ";
  print_string "différentes variables\n\n Pour le deuxième, il faut mettre ";
  print_string "le chemin du programme Polish que vous voulez analyser.\n";
;;

let main () =
  if Array.length (Sys.argv) > 3 then usage ()
  else match Sys.argv with
    | [|_; "-reprint"; file|] -> print_polish (read_polish file)
    | [|_; "-eval"; file|] -> eval_polish (read_polish file)
    | [|_; "-simpl"; file|] -> print_polish(simpl_polish (read_polish file))
    | [|_; "-vars"; file|] -> vars_polish (read_polish file)
    | [|_; "-sign"; file|] -> sign_polish (read_polish file)
    | _ -> usage ()
;;

(** Fonction principale, lancement du MAIN *)
let () = main ()

(*****************************************************************************)