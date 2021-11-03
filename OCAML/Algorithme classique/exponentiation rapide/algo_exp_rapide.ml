(*exponentiation rapide*)
let rec exp_rapide a n =
    if n = 0 then 1
    else let b = exp_rapide a (n/2) in (* pour éviter de faire 2 appels récursifs *)
    if n mod 2 = 0 then b*b
    else a*b*b;; (* si n est impair, n/2 et (n - 1)/2 ont la même valeur en OCaml donc on peut utiliser b aussi *)
