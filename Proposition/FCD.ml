(** Le module FCD contient les fonctions permettant de manipuler les formes
    clausales disjonctives. *)

open FC
open Formule

type fcd =
  | FCD of forme_clausale  (** Type d'une forme clausale disjonctive. *)

(** Calcule la conjonction de deux formes clausales disjonctives. *)
let fcd_conj (fc1 : fcd) (fc2 : fcd) =
  match (fc1, fc2) with
  | FCD fc1, FCD fc2 ->
      FCD
        (FormeClausale.fold
           (fun c1 acc ->
             FormeClausale.fold
               (fun c2 acc2 -> FormeClausale.add (Clause.union c1 c2) acc2)
               fc2 acc)
           fc1 FormeClausale.empty)

(** Calcule la disjonction de deux formes clausales disjonctives. *)
let fcd_disj (fc1 : fcd) (fc2 : fcd) =
  match (fc1, fc2) with
  | FCD fc1, FCD fc2 ->
      FCD
        (FormeClausale.of_list
           (FormeClausale.elements fc1 @ FormeClausale.elements fc2))

(** Mise en FCC, étape 3 : calcule la forme clausale disjonctive associée à une
    formule. *)
let rec forme_ensembliste (f : formule) : fcd =
  match f with
  | Atome a -> FCD (FormeClausale.singleton (Clause.singleton (Plus, a)))
  | Non (Atome a) -> FCD (FormeClausale.singleton (Clause.singleton (Moins, a)))
  | Et (a, b) -> fcd_conj (forme_ensembliste a) (forme_ensembliste b)
  | Ou (a, b) -> fcd_disj (forme_ensembliste a) (forme_ensembliste b)
  | Top -> FCD (FormeClausale.singleton Clause.empty)
  | Bot -> FCD FormeClausale.empty
  | _ -> failwith "Formule non traitable"

(** Convertit une formule en une forme clausale disjonctive équivalente. *)
let formule_to_fcd f = forme_ensembliste (descente_non (retrait_operateurs f))

(** Convertit une forme clausale disjonctive en une formule équivalente. *)
let fcd_to_formule (fcd : fcd) : formule =
  match fcd with
  | FCD fcd ->
      FormeClausale.fold
        (fun c acc ->
          let formule_clause =
            Clause.fold
              (fun lit acc_lit ->
                match lit with
                | Plus, a ->
                    if acc_lit = Bot then Atome a else Et (Atome a, acc_lit)
                | Moins, a ->
                    if acc_lit = Bot then Non (Atome a)
                    else Et (Non (Atome a), acc_lit))
              c Top
          in
          match acc with Bot -> formule_clause | _ -> Ou (acc, formule_clause))
        fcd Bot
