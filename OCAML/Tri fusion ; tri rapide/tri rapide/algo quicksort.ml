(*la fonction quicksort l utilise l'algorithme quicksort pour trier la liste l *)

let rec quicksort l = match l with
    | [] -> []
    | [e] -> [e]
    | e::q -> let l1, l2 = _partition e q in
        _concat (quicksort l1) (e::quicksort l2);;
