
(*renvoie une liste contenant les elements de l1 puis les elements de l2*)
let rec _concat l1 l2 = match l1 with
    | [] -> l2
    | e::q -> e::(_concat q l2);;

(*decoupe la liste en deux , d'un cote les elements plus grand que p et les autres*)
let rec _partition p l = match l with
    | [] -> ([], [])
    | e::q -> let l1, l2 = (_partition p q) in
                if e < p then ((e::l1), l2)
                else (l1, (e::l2));;


(*la fonction quicksort l utilise l'algorithme quicksort pour trier la liste l *)

let rec quicksort l = match l with
    | [] -> []
    | [e] -> [e]
    | e::q -> let l1, l2 = _partition e q in
        _concat (quicksort l1) (e::quicksort l2);;
