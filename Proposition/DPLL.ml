(** Le module DPLL contient les fonctions permettant de tester la
    satisfaisabilité d'une formule de la logique propositionnelle selon
    l'algorithme DPLL. *)

open FC
open FCC

(** Simplifie la forme clausale conjonctive fcc en considérant que le littéral
    lit est vrai *)
let simplif_fcc (fc : fcc) (lit : litteral) : fcc =
  match fc with
  | FCC fc ->
      FCC
        (FormeClausale.fold
           (fun elt acc ->
             if Clause.mem lit elt then acc
             else FormeClausale.add (Clause.remove (neg_lit lit) elt) acc)
           fc FormeClausale.empty)

(** Applique l'algorithme DPLL pour déterminer si une FCC est satisfaisable. *)
let rec dpll_sat (fc : fcc) : bool =
  match fc with
  | FCC fc ->
      if FormeClausale.is_empty fc then true
      else
        let c = FormeClausale.min_elt fc in
        if Clause.is_empty c then false
        else
          let l = Clause.min_elt c in
          dpll_sat (simplif_fcc (FCC fc) l)
          || dpll_sat (simplif_fcc (FCC fc) (neg_lit l))

(** Applique l'algorithme DPLL pour déterminer si une fcc est satisfaisable,
    renvoyant None si ce n'est pas le cas et Some res sinon, où res est une
    liste de couples (atome, Booléen) suffisants pour que la formule soit vraie.
*)
let rec dpll_ex_sat (fc : fcc) : (string * bool) list option =
  match fc with
  | FCC fc -> (
      if FormeClausale.is_empty fc then Some []
      else
        let c = FormeClausale.min_elt fc in
        if Clause.is_empty c then None
        else
          let l = Clause.min_elt c in
          match dpll_ex_sat (simplif_fcc (FCC fc) l) with
          | Some res -> Some ((snd l, fst l = Plus) :: res)
          | None -> (
              let l_neg = neg_lit l in
              match dpll_ex_sat (simplif_fcc (FCC fc) l_neg) with
              | Some res -> Some ((snd l_neg, fst l_neg = Plus) :: res)
              | None -> None))

(** Renvoie la liste des listes de couples (atome, Booléen) suffisants pour que
    la formule soit vraie, selon l'algorithme DPLL. *)
let rec dpll_all_sat (fc : fcc) : (string * bool) list list =
  match fc with
  | FCC fc ->
      if FormeClausale.is_empty fc then [ [] ]
      else
        let c = FormeClausale.min_elt fc in
        if Clause.is_empty c then []
        else
          let l = Clause.min_elt c in
          let res1 =
            List.map
              (fun sol -> (snd l, fst l = Plus) :: sol)
              (dpll_all_sat (simplif_fcc (FCC fc) l))
          in
          let l_neg = neg_lit l in
          let res2 =
            List.map
              (fun sol -> (snd l_neg, fst l_neg = Plus) :: sol)
              (dpll_all_sat (simplif_fcc (FCC fc) l_neg))
          in
          res1 @ res2

(** Applique l'algorithme DPLL pour déterminer si une fcc est satisfaisable.
    Utilise la propagation unitaire. *)
let rec dpll_sat_unit_prop (fc : fcc) : bool =
  match fc with
  | FCC fc -> (
      let c = FormeClausale.min_elt_opt fc in
      match c with
      | None -> true
      | Some cc when Clause.is_empty cc -> false
      | Some cc ->
          let l = Clause.min_elt cc in
          dpll_sat_unit_prop (simplif_fcc (FCC fc) l)
          || (not (Clause.cardinal cc = 1))
             && dpll_sat_unit_prop (simplif_fcc (FCC fc) (neg_lit l)))

(** Applique l'algorithme DPLL pour déterminer si une fcc est satisfaisable,
    renvoyant None si ce n'est pas le cas et Some res sinon, où res est une
    liste de couples (atome, Booléen) suffisants pour que la formule soit vraie.
    Utilise la propagation unitaire. *)
let rec dpll_ex_sat_unit_prop (fc : fcc) : (string * bool) list option =
  match fc with
  | FCC fc -> (
      let c = FormeClausale.min_elt_opt fc in
      match c with
      | None -> Some []
      | Some cc when Clause.is_empty cc -> None
      | Some cc -> (
          let l = Clause.min_elt cc in
          match dpll_ex_sat_unit_prop (simplif_fcc (FCC fc) l) with
          | Some res -> Some ((snd l, fst l = Plus) :: res)
          | None ->
              if not (Clause.cardinal cc = 1) then
                match
                  dpll_ex_sat_unit_prop (simplif_fcc (FCC fc) (neg_lit l))
                with
                | Some res ->
                    Some ((snd (neg_lit l), fst (neg_lit l) = Plus) :: res)
                | None -> None
              else None))

(** Renvoie la liste des listes de couples (atome, Booléen) suffisants pour que
    la formule soit vraie, selon l'algorithme DPLL. Utilise la propagation
    unitaire. *)
let rec dpll_all_sat_unit_prop (fc : fcc) : (string * bool) list list =
  match fc with
  | FCC fc -> (
      let c = FormeClausale.min_elt_opt fc in
      match c with
      | None -> [ [] ]
      | Some cc when Clause.is_empty cc -> []
      | Some cc ->
          let l = Clause.min_elt cc in
          let res1 =
            List.map
              (fun sol -> (snd l, fst l = Plus) :: sol)
              (dpll_all_sat_unit_prop (simplif_fcc (FCC fc) l))
          in
          if not (Clause.cardinal cc = 1) then
            let l_neg = neg_lit l in
            let res2 =
              List.map
                (fun sol -> (snd l_neg, fst l_neg = Plus) :: sol)
                (dpll_all_sat_unit_prop (simplif_fcc (FCC fc) l_neg))
            in
            res1 @ res2
          else res1)
