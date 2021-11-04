
(*pile*)

type 'a stack = {t : 'a array; mutable n : int}


(*implementation fonctionnelle*)

let stack_empty p = p = [];;

let stack_push p e = e::p;;

let stack_pop p = match p with
  | [] -> failwith "Pile vide"
  | e::q -> (e, q);;

(*implementation imperative*)

let stack_empty p = p.n = 0;; (* 0(1) *)

let stack_push p e = (* 0(1) *)
  if p.n >= Array.length p.t
  then failwith "Pile pleine"
  else (p.t.(p.n) <- e; p.n <- p.n + 1);;


let stack_pop p = (* 0(1) *)
  if p.n = 0 then failwith "Pile vide"
  else (p.n <- p.n - 1; p.t.(p.n))


(*file*)

type 'a queue_2lists = {l1 : 'a list; l2 : 'a list}

(*implementation persistante avec deux listes*)7

let queue_empty f = f.l1 = [] && f.l2 = [];;

let queue_add f e = {l1 = e::f.l1; l2 = f.l2};;
(* suppose que f est non vide *)
(* renvoie (élément, liste obtenue) *)
let rec queue_pop f = match f.l1 with
  | e::q -> e, {l1 = q; l2 = f.l2}
  |[] -> queue_pop {l1 = List.rev f.l2; l2 = []};;
  
(*implementation avec liste deoublement chainee*)

type 'a l2c = {
elem : 'a;
mutable prev : 'a l2c;
mutable next : 'a l2c
}

(*implémentation avec une liste chaînée *)
type 'a cell =
| Nil
| Cons of { content: 'a; mutable next: 'a cell }
type 'a t = {
mutable length: int;
mutable first: 'a cell;
mutable last: 'a cell
}

(*dictionnaire*)

(* exemple de dictionnaire avec une liste de couples *)
let d = [("red", (255, 0, 0)); ("black", (0, 0, 0))]
(*implementation par listes de couple*)
let add c l = c::l;;

let del k l =
List.filter (fun c -> fst c <> k) l;;

let get k l =
  List.filter (fun c -> fst c = k) l
    |> List.map snd;;



