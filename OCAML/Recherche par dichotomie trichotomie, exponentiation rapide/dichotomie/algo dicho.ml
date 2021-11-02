(*recherche par dichotomie*)

let dicho t e =
(* détermine si e appartient au tableau trié t *)
  let rec aux i j =
(* détermine si e appartient à t.(i), ..., t.(j) *)
    if i > j then false (* aucun élément *)
      else let m = (i + j)/2 in (* milieu *)
        if t.(m) = e then true
          else if t.(m) < e then aux (m + 1) j
              else aux i (m - 1) (* regarde à gauche *)
  in aux 0 (Array.length t - 1)
