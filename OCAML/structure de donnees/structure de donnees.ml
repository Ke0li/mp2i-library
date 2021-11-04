
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

(*implementation persistante avec deux listes*)

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

(*table de hachage*)

type ('k, 'v) hashtable = {
t : ('k * 'v) option array;
h : 'k -> int
};;

let hashtable_add ht (k, v) =
  ht.t.(ht.h k) <- Some v;;

let hashtable_get ht k =
  ht.t.(ht.h k);;

let hashtable_del ht k =
  ht.t.(ht.h k) <- None;;

(*implementation de dict*)

type ('k, 'v) dict = {
  add : 'k * 'v -> unit;
  del : 'k -> unit;
  get : 'k -> 'v option

* n est la taille du tableau à utiliser *)
let dict_of_hashtable n =

let ht = {
t = Array.make n None;
h = fun k -> k mod n
} in {
add = hashtable_add ht;
get = hashtable_get ht;
del = hashtable_del ht
}
}

(*ensemble*)
type 'a set = {
add : 'a -> unit;
del : 'a -> unit;
has : 'a -> bool
}


type ('k, 'v) dict = {
  add : 'k * 'v -> unit;
  del : 'k -> unit;
  get : 'k -> 'v option
}


let set_of_dict d = {
  add = (fun e -> d.add (e, 0));
  del = (fun e -> d.del e);
  get = (fun e -> match d.get e with
    | None -> false
    | Some _ -> true)
}
