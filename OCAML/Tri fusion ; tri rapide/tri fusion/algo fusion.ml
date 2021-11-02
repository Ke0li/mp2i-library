(*algo tri fusion*)

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
