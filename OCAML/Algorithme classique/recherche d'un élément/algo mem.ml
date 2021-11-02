

(*recherche d'un élément*)
let rec mem l e = match l with
    | [] -> false
    | x::q -> x = e || mem q e;;
