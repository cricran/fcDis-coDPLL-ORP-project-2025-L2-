(** Le module FCC contient les fonctions permettant de manipuler les formes
    clausales conjonctives. *)

open FC
open Formule

type fcc =
  | FCC of forme_clausale  (** Type d'une forme clausale conjonctive. *)

(** Calcule la conjonction de deux formes clausales conjonctives. *)
let fcc_conj (fc1 : fcc) (fc2 : fcc) : fcc =
  match (fc1, fc2) with
  | FCC fc1, FCC fc2 ->
      FCC
        (FormeClausale.of_list
           (FormeClausale.elements fc1 @ FormeClausale.elements fc2))

(** Calcule la disjonction de deux formes clausales conjonctives. *)
let fcc_disj (fc1 : fcc) (fc2 : fcc) : fcc =
  match (fc1, fc2) with
  | FCC fc1, FCC fc2 ->
      FCC
        (FormeClausale.fold
           (fun c1 acc ->
             FormeClausale.fold
               (fun c2 acc2 -> FormeClausale.add (Clause.union c1 c2) acc2)
               fc2 acc)
           fc1 FormeClausale.empty)

(** Mise en FCC, étape 3 : calcule la forme clausale conjonctive associée à une
    formule. *)
let rec forme_ensembliste (f : formule) : fcc =
  match f with
  | Atome a -> FCC (FormeClausale.singleton (Clause.singleton (Plus, a)))
  | Non (Atome a) -> FCC (FormeClausale.singleton (Clause.singleton (Moins, a)))
  | Et (a, b) -> fcc_conj (forme_ensembliste a) (forme_ensembliste b)
  | Ou (a, b) -> fcc_disj (forme_ensembliste a) (forme_ensembliste b)
  | Top -> FCC FormeClausale.empty
  | Bot -> FCC (FormeClausale.singleton Clause.empty)
  | _ -> failwith "Formule non traitable"

(** Convertit une formule en une forme clausale conjonctive équivalente.*)
let formule_to_fcc f = forme_ensembliste (descente_non (retrait_operateurs f))

(** Convertit une forme clausale conjonctive en une formule équivalente. *)
let fcc_to_formule (fcc : fcc) : formule =
  match fcc with
  | FCC fcc ->
      FormeClausale.fold
        (fun c acc ->
          let formule_clause =
            Clause.fold
              (fun lit acc_lit ->
                match lit with
                | Plus, a ->
                    if acc_lit = Top then Atome a else Ou (Atome a, acc_lit)
                | Moins, a ->
                    if acc_lit = Top then Non (Atome a)
                    else Ou (Non (Atome a), acc_lit))
              c Bot
          in
          match acc with Top -> formule_clause | _ -> Et (acc, formule_clause))
        fcc Top

(* Ajouter dans ce fichier les fonctions nécessaires à sa réalisation *)
