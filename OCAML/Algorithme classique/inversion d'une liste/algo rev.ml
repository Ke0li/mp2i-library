
(*inversion d'une liste avec un accumulateur*)


let rec rev acc l = match l with  (* acc va servir à construire le résultat (la liste à l'envers) *)
  | [] -> acc
  | e::q -> rev (e::acc) q;;
