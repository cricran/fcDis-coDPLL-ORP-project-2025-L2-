(** Le module CoDPLL contient les fonctions permettant de tester la satisfaisabilité
    d'une formule de la logique propositionnelle selon l'algorithme CoDPLL. *)

open FC
open FCD

(** Simplifie la forme clausale disjonctive fcd en considérant que le littéral lit est vrai *)
let simplif_fcd (_ : fcd) : litteral -> fcd = failwith "simplif_fcd : à faire"

(** Applique l'algorithme CoDPLL pour déterminer si une fcd est satisfaisable. *)
let codpll_sat (_ : fcd) : bool = failwith "codpll_sat : à faire"

(** Applique l'algorithme CoDPLL pour déterminer si une fcd est satisfaisable, renvoyant None si ce n'est pas le cas
      et Some res sinon, où res est une liste de couples (atome, Booléen)
      suffisants pour que la formule soit vraie. *)
let codpll_ex_sat (_ : fcd) : (string * bool) list option =
  failwith "codpll_ex_sat : à faire"

(** Renvoie la liste des listes de couples (atome, Booléen) suffisants pour que la formule soit vraie,
    selon l'algorithme CoDPLL. *)
let codpll_all_sat (_ : fcd) : (string * bool) list list =
  failwith "codpll_all_sat : à faire"
