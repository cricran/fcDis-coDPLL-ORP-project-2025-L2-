(** Le module FC contient les types et définitions de base permettant de
    calculer les étapes communes aux mises en forme clausale conjonctive et
    disjonctive de formules de la logique propositionnelle. *)

open Formule

(** Signe d'un littéral. *)
type signe = Plus | Moins

type litteral = signe * string
(** Type d'un littéral : produit d'un signe et d'un atome (string). *)

(** Inversion d'un signe.*)
let neg_sign (s : signe) : signe = match s with Plus -> Moins | Moins -> Plus

(** Inversion du signe d'un littéral.*)
let neg_lit (l : litteral) : litteral =
  match l with s, str -> (neg_sign s, str)

(** Le module Clause permet de manipuler les ensembles de littéraux. Il est
    généré via le foncteur Set.Make. *)
module Clause = Set.Make (struct
  type t = litteral

  let compare = Stdlib.compare
end)

type clause = Clause.t
(** Type synonyme : une clause est un ensemble de littéraux. *)

(** Le module FormeClausale permet de manipuler les ensembles de clauses. Il est
    généré via le foncteur Set.Make. *)
module FormeClausale = Set.Make (struct
  type t = clause

  let compare x y =
    let c = Stdlib.compare (Clause.cardinal x) (Clause.cardinal y) in
    if c <> 0 then c else Clause.compare x y
end)

type forme_clausale = FormeClausale.t
(** Type synonyme : une forme clausale est un ensemble de clauses. *)

(** Renvoie la liste des littéraux d'une clause. *)
let clause_to_list (c : clause) : litteral list = Clause.elements c

(** Transforme un littéral en string. *)
let string_of_lit (l : litteral) : string =
  match l with Moins, a -> "¬" ^ a | Plus, a -> a

(** Transforme une clause en string. *)
let string_of_clause (c : clause) : string =
  "{" ^ String.concat ", " (List.map string_of_lit (clause_to_list c)) ^ "}"

(** Renvoie une string représentant une forme clausale *)
let string_of_fc (fc : forme_clausale) : string =
  "{"
  ^ String.concat ", " (List.map string_of_clause (FormeClausale.elements fc))
  ^ "}"

(** Mise en FC, étape 1 : Transforme une formule en une formule équivalente avec
    des opérateurs de conjonction, de disjonction, de négation, Bot et Top
    uniquement. *)
let rec retrait_operateurs (f : formule) : formule =
  match f with
  | Non a -> Non (retrait_operateurs a)
  | Ou (ad, ag) -> Ou (retrait_operateurs ad, retrait_operateurs ag)
  | Et (ad, ag) -> Et (retrait_operateurs ad, retrait_operateurs ag)
  | Imp (ad, ag) -> Ou (Non (retrait_operateurs ad), retrait_operateurs ag)
  | Equiv (ad, ag) ->
      Ou
        ( Et (Non (retrait_operateurs ad), Non (retrait_operateurs ag)),
          Et (retrait_operateurs ag, retrait_operateurs ad) )
  | _ -> f

(** Mise en FC, étape 2 : Descend les négations dans une formule au plus profond
    de l'arbre syntaxique, en préservant les évaluations. *)
let rec descente_non (f : formule) : formule =
  match f with
  | Non (Et (ad, ag)) -> Ou (descente_non (Non ad), descente_non (Non ag))
  | Non (Ou (ad, ag)) -> Et (descente_non (Non ad), descente_non (Non ag))
  | Non (Non a) -> descente_non a
  | Non Bot -> Top
  | Non Top -> Bot
  | Et (ad, ag) -> Et (descente_non ad, descente_non ag)
  | Ou (ad, ag) -> Ou (descente_non ad, descente_non ag)
  | _ -> f
(* Ajouter dans ce fichier les fonctions nécessaires à sa réalisation *)
