open Formule
open RandomFormule
open FCD
open FCC
open Quine
open FC

(** correct_ex_sat res f : Teste si un témoin de satisfaisabilité est correct :
    - si le témoin (res) est None, teste si la formule est une contradiction
      avec l'algorithme de Quine
    - sinon, si le témoin (res) est (Some l), teste si la formule est vraie pour
      l'interprétation obtenue depuis l *)
let correct_ex_sat (_ : (string * bool) list option) (_ : formule) : bool =
  failwith "correct_ex_sat : à faire"

(** correct_all_sat res f : Teste si les témoins de satisfaisabilité sont
    corrects. Plus précisément, teste si pour chaque sous-liste l2 d'atomes de
    f, l'évaluation de f avec l'interprétation obtenue depuis l2 est équivalente
    à l'existence d'une sous-liste l1 de res telle que pour tout atome (at, b)
    de l1, b est vrai si (at est dans l1) *)
let correct_all_sat (_ : (string * bool) list list) (_ : formule) : bool =
  failwith "correct_all_sat : à faire"

(** Teste si les algorithmes du projet sont corrects pour une formule f donnée.
*)
let test_log f =
  Printf.printf "Formule générée : %s\n" (string_of_formule f);

  let (FCD fc_d as fcd) = formule_to_fcd f
  and (FCC fc_c as fcc) = formule_to_fcc f in

  let f' = fcd_to_formule fcd and f'' = fcc_to_formule fcc in

  Printf.printf "Forme clausale disjonctive : %s\n" (string_of_fc fc_d);
  Printf.printf "Forme clausale conjonctive : %s\n" (string_of_fc fc_c);

  Printf.printf "Formule de la Forme clausale disjonctive : %s\n"
    (string_of_formule f');
  Printf.printf "Formule de la Forme clausale conjonctive : %s\n"
    (string_of_formule f'');

  Printf.printf "La mise en FCD préserve la sémantique : %b\n"
    (quine_tauto (Equiv (f, f')));
  Printf.printf "La mise en FCC préserve la sémantique : %b\n"
    (quine_tauto (Equiv (f, f'')))

(* Printf.printf "Équisatisfaisabilité entre Quine et DPLL : %b\n"
    (quine_sat f = DPLL.dpll_sat fcc);
  Printf.printf "Équisatisfaisabilité entre Quine et DPLL (unit prop) : %b\n"
    (quine_sat f = DPLL.dpll_sat_unit_prop fcc);
  Printf.printf "Correction de Dpll_ex_sat : %b\n"
    (correct_ex_sat (DPLL.dpll_ex_sat fcc) f);
  Printf.printf "Correction de Dpll_ex_sat_unit_prop : %b\n"
    (correct_ex_sat (DPLL.dpll_ex_sat_unit_prop fcc) f);
  Printf.printf "Correction de Dpll_all_sat : %b\n"
    (correct_all_sat (DPLL.dpll_all_sat fcc) f);
  Printf.printf "Correction de Dpll_all_sat_unit_prop : %b\n"
    (correct_all_sat (DPLL.dpll_all_sat_unit_prop fcc) f);

  Printf.printf "Équisatisfaisabilité entre Quine et CoDPLL : %b\n"
    (quine_sat f = CoDPLL.codpll_sat fcd);
  Printf.printf "Correction de coDpll_ex_sat : %b\n"
    (correct_ex_sat (CoDPLL.codpll_ex_sat fcd) f);
  Printf.printf "Correction de CoDpll_all_sat : %b\n"
    (correct_all_sat (CoDPLL.codpll_all_sat fcd) f) *)

let rand_test_log () = test_log (random_form [ "a"; "b"; "c" ] 3)
