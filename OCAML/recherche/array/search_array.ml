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
  
  (*recherche par trichotomie*)
let tricho t e =
  let rec aux i j =
    if i > j then false
      else let m1 = (2*i + j + 1)/3 in
      let m2 = (i + 2*j + 2)/3 in
      if t.(m1) = e || t.(m2) = e then true
        else if e < t.(m1) then aux i (m1 - 1)
        else if e < t.(m2) then aux (m1 + 1) (m2 - 1)
        else aux (m2 + 1) j in
  aux 0 (Array.length t - 1)
  
  
(* renvoie un maximum local*)
let max_local t = 
    let n = Array.length t in
    let rec aux i j =
        let m = (i + j)/2 in
        if (m = 0 || t.(m) >= t.(m-1)) && (m = n - 1 || t.(m) >= t.(m+1))
        then m
        else if t.(m) < t.(m - 1) then aux i (m - 1)
        else aux (m + 1) j in
    aux 0 (n + 1);;


(* recherche de la tranche maximum dans un tableau *)
let tranche_max t =
    let m = ref t.(0) in
    let m_cur = ref t.(0) in
    for i = 1 to Array.length t - 1 do
        m_cur := max (!m_cur + t.(i)) t.(i);
        m := max !m !m_cur
    done;
    !m;;
    
