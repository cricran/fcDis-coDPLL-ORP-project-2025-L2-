(** Le module RandomFormule contient une fonction permettant de générer
    aléatoirement une formule de la logique propositionnelle. *)

open Formule

(** random_atom atoms renvoit un atome aléatoire parmie la liste d'atoms atoms.
*)
let random_atom (atoms : string list) : formule =
  Atome (List.nth atoms (Random.int (List.length atoms)))

(** random_form atoms n renvoie une formule aléatoire possédant n opérateurs et
    des atomes parmi atoms. *)
let random_form (atoms : string list) (n : int) : formule =
  Random.self_init ();
  let rec aux acc =
    match acc with
    | 0 -> random_atom atoms
    | 1 -> (
        match Random.int 7 with
        | 0 -> Top
        | 1 -> Bot
        | 2 -> Non (random_atom atoms)
        | 3 -> Ou (random_atom atoms, random_atom atoms)
        | 4 -> Et (random_atom atoms, random_atom atoms)
        | 5 -> Imp (random_atom atoms, random_atom atoms)
        | 6 -> Equiv (random_atom atoms, random_atom atoms)
        | _ -> failwith "random_form : erreur dans la génération de la formule")
    | n -> (
        match Random.int 5 with
        | 0 -> Non (aux (n - 1))
        | 1 ->
            let k = Random.int (n - 1) in
            let l = n - k - 1 in
            Ou (aux k, aux l)
        | 2 ->
            let k = Random.int (n - 1) in
            let l = n - k - 1 in
            Et (aux k, aux l)
        | 3 ->
            let k = Random.int (n - 1) in
            let l = n - k - 1 in
            Imp (aux k, aux l)
        | 4 ->
            let k = Random.int (n - 1) in
            let l = n - k - 1 in
            Equiv (aux k, aux l)
        | _ -> failwith "random_form : erreur dans la génération de la formule")
  in
  aux n
