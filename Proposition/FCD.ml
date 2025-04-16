(** Le module FCD contient les fonctions permettant de manipuler les formes
    clausales disjonctives. *)

open FC
open Formule

type fcd =
  | FCD of forme_clausale  (** Type d'une forme clausale disjonctive. *)

(** Calcule la conjonction de deux formes clausales disjonctives. *)
let fcd_conj (_ : fcd) (_ : fcd) = failwith "fcd_conj : à faire"

(** Calcule la disjonction de deux formes clausales disjonctives. *)
let fcd_disj (_ : fcd) (_ : fcd) = failwith "fcd_disj : à faire"

(** Mise en FCC, étape 3 : calcule la forme clausale disjonctive associée à une
    formule. *)
let forme_ensembliste (_ : formule) : fcd =
  failwith "forme_ensembliste : à faire"

(** Convertit une formule en une forme clausale disjonctive équivalente.*)
let formule_to_fcd f = forme_ensembliste (descente_non (retrait_operateurs f))

(** Convertit une forme clausale disjonctive en une formule équivalente. *)
let fcd_to_formule (_ : fcd) : formule = failwith "fcd_to_formule : à faire"

(* Ajouter dans ce fichier les fonctions nécessaires à sa réalisation *)
