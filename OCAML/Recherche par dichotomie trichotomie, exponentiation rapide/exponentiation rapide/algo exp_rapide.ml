(*exponentiation rapide*)
let rec exp_rapide a n =
  if n = 0 then 1
  else let b = exp_rapide a (n/2) in
  if n mod 2 = 0 then b*b
  else a*b*b
