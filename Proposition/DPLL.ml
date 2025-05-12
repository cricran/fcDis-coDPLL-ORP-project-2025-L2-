(** Le module DPLL contient les fonctions permettant de tester la satisfaisabilité
    d'une formule de la logique propositionnelle selon l'algorithme DPLL. *)

open FC
open FCC

(** Simplifie la forme clausale conjonctive fcc en considérant que le littéral lit est vrai *)
let simplif_fcc (_ : fcc) : litteral -> fcc = failwith "simplif_fcc : à faire"

(** Applique l'algorithme DPLL pour déterminer si une FCC est satisfaisable. *)
let dpll_sat (_ : fcc) : bool = failwith "dpll_sat : à faire"

(** Applique l'algorithme DPLL pour déterminer si une fcc est satisfaisable, renvoyant None si ce n'est pas le cas
      et Some res sinon, où res est une liste de couples (atome, Booléen)
      suffisants pour que la formule soit vraie. *)
let dpll_ex_sat (_ : fcc) : (string * bool) list option =
  failwith "dpll_ex_sat : à faire"

(** Renvoie la liste des listes de couples (atome, Booléen) suffisants pour que la formule soit vraie,
    selon l'algorithme DPLL. *)
let dpll_all_sat (_ : fcc) : (string * bool) list list =
  failwith "dpll_all_sat : à faire"

(** Applique l'algorithme DPLL pour déterminer si une fcc est satisfaisable. 
    Utilise la propagation unitaire. *)
let dpll_sat_unit_prop (_ : fcc) : bool =
  failwith "dpll_sat_unit_prop : à faire"

(** Applique l'algorithme DPLL pour déterminer si une fcc est satisfaisable, renvoyant None si ce n'est pas le cas
      et Some res sinon, où res est une liste de couples (atome, Booléen)
      suffisants pour que la formule soit vraie.
      Utilise la propagation unitaire. *)
let dpll_ex_sat_unit_prop (_ : fcc) : (string * bool) list option =
  failwith "dpll_ex_sat_unit_prop : à faire"

(** Renvoie la liste des listes de couples (atome, Booléen) suffisants pour que la formule soit vraie,
    selon l'algorithme DPLL.
    Utilise la propagation unitaire. *)
let dpll_all_sat_unit_prop (_ : fcc) : (string * bool) list list =
  failwith "dpll_all_sat_unit_prop : à faire"
