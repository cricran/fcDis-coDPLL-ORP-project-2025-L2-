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

(** Transforme une liste de couples string*bool en une interprétation. *)
let interpretation_of_map (l : (string * bool) list) : interpretation = function
  | s -> (
      match List.find_opt (fun (x, _) -> x = s) l with
      | Some (_, b) -> b
      | None -> false)

(** Transforme une liste de couples string en une interprétation. *)
let interpretation_of_list (l : string list) : interpretation = function
  | s -> List.mem s l

(** Calcule la liste (triée et sans doublon) des atomes d'une formule.*)
let rec atomes (f : formule) : string list =
  match f with
  | Atome s -> [ s ]
  | Imp (sg, sd) | Ou (sg, sd) | Et (sg, sd) | Equiv (sg, sd) ->
      atomes sg @ atomes sd
  | Non sa -> atomes sa
  | Bot | Top -> []

(** Calcule la liste de toutes les sous-listes d'une liste donnée. *)
let rec all_sublists lst =
  match lst with
  | [] -> [ [] ]
  | hd :: tl ->
      let subNoHd = all_sublists tl in
      let subHd = List.map (fun sub -> hd :: sub) subNoHd in
      subNoHd @ subHd
