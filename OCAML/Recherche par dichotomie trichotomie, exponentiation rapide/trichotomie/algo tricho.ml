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
