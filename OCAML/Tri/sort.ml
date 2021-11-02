
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


(* tri utilisant un comptage*)
let tri_comptage t =
    let m = _maximum t in
    let compte = Array.make (m+1) 0 in
    let n = Array.length t in
    for i=0 to n - 1 do
        compte.(t.(i)) <- compte.(t.(i)) + 1
    done;
    let k = ref 0 in
    for i=0 to m do
        for j=1 to compte.(i) do
            t.(!k) <- i;
            incr k
        done
    done;;
    
    
(*algo pour diviser une liste en deux*)
let rec split = function
  | [] -> [], []
  | [e] -> [e], []
  | e1::e2::q -> let q1, q2 = split q in
  e1::q1, e2::q2

(*algo pour fusionner deux listes triées*)
let rec fusion l1 l2 = match l1, l2 with
  | [], _ -> l2
  | _, [] -> l1
  | e1::q1, e2::q2 when e1 < e2 -> e1::fusion q1 l2
  | e1::q1, e2::q2 -> e2::fusion l1 q2

(*algo tri fusion*)
let rec tri = function
  | [] -> []
  | [e] -> [e] (* tri ne termine pas sans ce cas *)
  | l -> let l1, l2 = split l in
fusion (tri l1) (tri l2);;
