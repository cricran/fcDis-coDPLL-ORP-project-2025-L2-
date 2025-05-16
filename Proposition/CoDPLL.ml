(** Le module CoDPLL contient les fonctions permettant de tester la
    satisfaisabilité d'une formule de la logique propositionnelle selon
    l'algorithme CoDPLL. *)

open FC
open FCD

(** Simplifie la forme clausale disjonctive fcd en considérant que le littéral
    lit est vrai *)
let simplif_fcd (fd : fcd) (lit : litteral) : fcd =
  match fd with
  | FCD fd ->
      FCD
        (FormeClausale.fold
           (fun elt acc ->
             if Clause.mem (neg_lit lit) elt then acc
             else FormeClausale.add (Clause.remove lit elt) acc)
           fd FormeClausale.empty)

(** Applique l'algorithme CoDPLL pour déterminer si une fcd est satisfaisable.
*)
let rec codpll_sat (fd : fcd) : bool =
  match fd with
  | FCD fd ->
      if FormeClausale.is_empty fd then false
      else
        let c = FormeClausale.min_elt fd in
        if Clause.is_empty c then true
        else
          let l = Clause.min_elt c in
          codpll_sat (simplif_fcd (FCD fd) l)
          || codpll_sat (simplif_fcd (FCD fd) (neg_lit l))

(** Applique l'algorithme CoDPLL pour déterminer si une fcd est satisfaisable,
    renvoyant None si ce n'est pas le cas et Some res sinon, où res est une
    liste de couples (atome, Booléen) suffisants pour que la formule soit vraie.
*)
let rec codpll_ex_sat (fd : fcd) : (string * bool) list option =
  match fd with
  | FCD fd -> (
      if FormeClausale.is_empty fd then None
      else
        let c = FormeClausale.min_elt fd in
        if Clause.is_empty c then Some []
        else
          let l = Clause.min_elt c in
          match codpll_ex_sat (simplif_fcd (FCD fd) l) with
          | Some res -> Some ((snd l, fst l = Plus) :: res)
          | None -> (
              let l_neg = neg_lit l in
              match codpll_ex_sat (simplif_fcd (FCD fd) l_neg) with
              | Some res -> Some ((snd l_neg, fst l_neg = Plus) :: res)
              | None -> None))

(** Renvoie la liste des listes de couples (atome, Booléen) suffisants pour que
    la formule soit vraie, selon l'algorithme CoDPLL. *)
let rec codpll_all_sat (fd : fcd) : (string * bool) list list =
  match fd with
  | FCD fd ->
      if FormeClausale.is_empty fd then []
      else
        let c = FormeClausale.min_elt fd in
        if Clause.is_empty c then [ [] ]
        else
          let l = Clause.min_elt c in
          let res1 =
            List.map
              (fun sol -> (snd l, fst l = Plus) :: sol)
              (codpll_all_sat (simplif_fcd (FCD fd) l))
          in
          let l_neg = neg_lit l in
          let res2 =
            List.map
              (fun sol -> (snd l_neg, fst l_neg = Plus) :: sol)
              (codpll_all_sat (simplif_fcd (FCD fd) l_neg))
          in
          res1 @ res2
