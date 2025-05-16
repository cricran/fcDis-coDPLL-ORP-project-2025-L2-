(** Le module Quine contient les fonctions permettant de tester la
    satisfaisabilité d'une formule de la logique propositionnelle selon
    l'algorithme de Quine. *)

open Formule

(** subst g s f : substitue une formule g à un atome s dans une formule f. *)
let rec subst : formule -> string -> formule -> formule =
 fun g s f ->
  match f with
  | Atome a when a = s -> g
  | Imp (sg, sd) -> Imp (subst g s sg, subst g s sd)
  | Ou (sg, sd) -> Ou (subst g s sg, subst g s sd)
  | Et (sg, sd) -> Et (subst g s sg, subst g s sd)
  | Equiv (sg, sd) -> Equiv (subst g s sg, subst g s sd)
  | Non sf -> Non (subst g s sf)
  | _ -> f

(** Choisit un atome d'une formule, renvoyant None si aucun n'est présent.*)
let rec choix_atome : formule -> string option = function
  | Atome a -> Some a
  | Imp (sg, sd) | Ou (sg, sd) | Et (sg, sd) | Equiv (sg, sd) ->
      let r = choix_atome sg in
      if r = None then choix_atome sd else r
  | Non sf -> choix_atome sf
  | _ -> None

(** Simplifie une formule d'une manière paresseuse. *)
let rec simplif_quine : formule -> formule = function
  | Ou (f, g) -> (
      match simplif_quine f with
      | Bot -> simplif_quine g
      | Top -> Top
      | f' -> (
          match simplif_quine g with Bot -> f' | Top -> Top | g' -> Ou (f', g'))
      )
  | Et (f, g) -> (
      match simplif_quine f with
      | Bot -> Bot
      | Top -> simplif_quine g
      | f' -> (
          match simplif_quine g with Bot -> Bot | Top -> f' | g' -> Et (f', g'))
      )
  | Imp (f, g) -> (
      match simplif_quine f with
      | Bot -> Top
      | Top -> simplif_quine g
      | f' -> (
          match simplif_quine g with
          | Bot -> Non f
          | Top -> Top
          | g' -> Imp (f', g')))
  | Equiv (f, g) -> (
      match simplif_quine f with
      | Bot -> simplif_quine (Non g)
      | Top -> simplif_quine g
      | f' -> (
          match simplif_quine g with
          | Bot -> Non f'
          | Top -> f'
          | g' -> Equiv (f', g')))
  | Non f -> (
      match simplif_quine f with Bot -> Top | Top -> Bot | f' -> Non f')
  | Top -> Top
  | Bot -> Bot
  | f' -> f'

(** Teste si une formule est satisfaisable, selon l'algorithme de Quine. *)
let rec quine_sat : formule -> bool =
 fun f ->
  match choix_atome f with
  | None -> ( match simplif_quine f with Top -> true | _ -> false)
  | Some a ->
      quine_sat (simplif_quine (subst Bot a f))
      || quine_sat (simplif_quine (subst Top a f))

(** Teste si une formule est une tautologie, selon l'algorithme de Quine. *)
let rec quine_tauto : formule -> bool =
 fun f ->
  match choix_atome f with
  | None -> ( match simplif_quine f with Top -> true | _ -> false)
  | Some a ->
      quine_tauto (simplif_quine (subst Bot a f))
      && quine_tauto (simplif_quine (subst Top a f))

(** Teste si une formule est une contradiction, selon l'algorithme de Quine. *)
let quine_contra : formule -> bool = fun f -> not (quine_sat f)

(* Ajouter dans ce fichier les fonctions nécessaires à sa réalisation *)
