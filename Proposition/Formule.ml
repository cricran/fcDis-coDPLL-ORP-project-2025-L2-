(** Le module Formule contient les types et définitions de base permettant la
    manipulation des formules de la logique propositionnelle. *)

(** Type des formules de la logique propositionnelle, avec des string comme
    atomes. *)
type formule =
  | Bot
  | Top
  | Atome of string
  | Imp of (formule * formule)
  | Ou of (formule * formule)
  | Et of (formule * formule)
  | Non of formule
  | Equiv of (formule * formule)

(** Conversion d'une formule en chaîne de caractères. *)
let rec string_of_formule (f : formule) : string =
  match f with
  | Bot -> "⊥"
  | Top -> "T"
  | Atome s -> s
  | Imp (sg, sd) ->
      "(" ^ string_of_formule sg ^ " → " ^ string_of_formule sd ^ ")"
  | Ou (sg, sd) ->
      "(" ^ string_of_formule sg ^ " ∨ " ^ string_of_formule sd ^ ")"
  | Et (sg, sd) ->
      "(" ^ string_of_formule sg ^ " ∧ " ^ string_of_formule sd ^ ")"
  | Equiv (sg, sd) ->
      "(" ^ string_of_formule sg ^ " ↔ " ^ string_of_formule sd ^ ")"
  | Non sa -> " ¬ " ^ string_of_formule sa

type interpretation = string -> bool
(** Type des interprétations. *)

(** Évalue une formule en fonction d'une interprétation. *)
let rec eval (i : interpretation) (f : formule) : bool =
  match f with
  | Bot -> false
  | Top -> true
  | Atome s -> i s
  | Imp (sg, sd) -> (not (eval i sg)) || eval i sd
  | Ou (sg, sd) -> eval i sg || eval i sd
  | Et (sg, sd) -> eval i sg && eval i sd
  | Equiv (sg, sd) -> eval i sg = eval i sd
  | Non sa -> not (eval i sa)

(* Ajouter dans ce fichier les fonctions nécessaires à sa réalisation *)
