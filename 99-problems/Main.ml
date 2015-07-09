open Core.Std
(*
module Let = struct
	type t = A | B | C
		with compare, sexp
end

include Comparable.Make(Let)
*)

(*--------------------------*)
(* 99 problems  *)

(* 1 *)
let rec last = function
	| [] -> None
	| hd :: [] -> Some hd
	| _ :: tl -> last tl 

(* 2 *)
let rec last_two = function
	| [] | [_] -> None	
	| [a; b]   -> Some (a, b)
	| _ :: tl  -> last_two tl

(* 3 *)
let rec at k = function
	| [] -> None
	| x::xs -> if k = 0 then Some x else at (k - 1) xs

(* 4 *)
let length list = 
	let rec loop res = function
		| [] -> res
		| _::xs -> loop (res + 1) xs
	in loop 0 list

(* 5 *)
let rev list = 
	let rec loop res = function
		| [] -> res
		| x::xs -> loop (x::res) xs
	in loop [] list

(* 6 *)
let is_palindrome list = list = rev list

(* 7 *)
type 'a node =
	| One of 'a 
    | Many of 'a node list

(* 8 *)
let flatten list = 
	let rec aux acc = function
		| [] -> acc
		| One x :: t -> aux (x :: acc) t
		| Many x :: t -> aux (aux acc x) t
	in List.rev (aux [] list)

(* 9 *)
let compress list = 
	let rec aux acc = function
		| [] -> acc
		| [x] -> x :: acc
		| a::b::t -> if a = b then aux acc (b::t) else aux (a :: acc) (b::t)
	in List.rev (aux [] list)

(* 10 *)
let pack list = 
	let rec aux sub acc = function
		| [] -> acc
		| [x] -> (x :: sub) :: acc
		| a::b::t -> if a = b then aux (a :: sub) acc (b::t) else aux [] ((a :: sub) :: acc) (b::t)
	in List.rev (aux [] [] list)

(* 11 *)

let encode list = 
	pack list |> List.map ~f:(fun xs -> (length xs, List.hd_exn xs))

(* 12 *)
type 'a rle =
    | One of 'a
    | Many of int * 'a

 let encode2 list = 
 	let go xs = 
 		let len = List.length xs in
 		let hd  = List.hd_exn xs in
 		if len = 1 then One hd else Many (len, hd)
 	in pack list |> List.map ~f:go

(* 13 *)
let decode list = 
	let replicate n a = List.init n ~f:(const a) in
	let go = function
		| One x -> [x]
		| Many (n, x) -> replicate n x 
	in
	let rec aux acc = function
		| [] -> acc
		| [] :: tl -> aux acc tl
		| (h :: t) :: t2 -> aux (h :: acc) (t :: t2)
	in List.rev (aux [] (List.map ~f:go list))

(* 14 *)

let encode3 list = 
	let last count elem = if count = 0 then One elem else Many (count + 1, elem) in
	let rec aux count acc = function
		| [] -> acc
		| [x] -> last count x :: acc
		| a::b::t -> if a = b then aux (count + 1) acc (b::t) else aux 0 ((last count a) :: acc) (b::t)
	in List.rev (aux 0 [] list)

(* 15 *)

let duplicate list = List.bind list (fun x -> [x; x])
	
(* 16 *)

let replicate list n =
	let aux n a = List.init n ~f:(const a) in 
	List.bind list (aux n)

(* 17 *)
let drop list n = 
	let rec aux count acc = function
		| [] -> acc
		| h::t -> if (count = 1) then aux n acc t else aux (count - 1) (h :: acc) t
	in List.rev (aux n [] list)

(* 18 *)
let split list n = 
	let rec aux acc count = function
		| [] -> (acc, [])
		| t when count = 0 -> (acc, t)
		| h::t -> aux (h :: acc) (count - 1) t
	in let (x1, x2) = aux [] n list in (List.rev x1, x2)

(* 19 *)
let slice list m n =
	let list1 = snd (split list m)
	in  fst (split list1 (n - m))

(* 20 *)
let rotate list n = 	
	let a, b = split list (n % List.length list) in
	List.append b a
	
(* 21 *)
let remove_at n list = 
	let a, b = split list n in
	match b with
		| [] 		-> a
		| _::tl 	-> List.append a tl

(* 22 *)
let insert_at elem n list =
	let a, b = split list n in
	List.append a (elem :: b)

(* 23 *)
let range m n = 
	let sign = if (m < n) then 1 else (-1) in
	List.init (abs (m - n) + 1) ~f:(fun x -> m + x * sign)

(* 24 *)
let rand_select list n =	
	let rec aux acc list len = function
		| 0  -> acc		
		| count ->
			if List.is_empty list 
			then acc 
			else
				let idx = Random.int len in
				aux (List.nth_exn list idx :: acc) (remove_at idx list) (len - 1) (count - 1)
	in aux [] list (List.length list) n

(* 25 *)
let lotto_select n m = rand_select (range 1 m) n

(* 26 *)
let permutation list = rand_select list (List.length list)

(* 27 *)
let rec extract_no_dup n list = match n with
	| 1 -> List.map list ~f:List.return
	| _ -> 
		let elem_and_rest = List.init (List.length list - 1) ~f:(fun n -> let xs = snd (split list n) in (List.hd_exn xs, List.tl_exn xs)) in
		List.bind elem_and_rest (fun (elem, rest) -> List.map (extract_no_dup (n-1) rest) ~f:(fun xs -> elem :: xs))

let extract n list = extract_no_dup n (List.dedup list)

(* 28: Group the elements of a set into disjoint subsets *)
(* skip *)

(* 29 *)

let length_sort list = 
	let xs = List.map list ~f:(fun x -> (List.length x, x)) in
	List.sort ~cmp:(fun a b ->  Int.compare (fst a) (fst b)) xs |> List.map ~f:snd

let frequency_sort list = 
	let lens = List.map ~f:List.length list |> List.sort ~cmp:Int.compare |> encode in
	let get_frequency xs = 
		let len = List.length xs in List.find_exn lens ~f:(fun x -> len = snd x) |> fst
	in
	let xs = List.map list ~f:(fun x -> (get_frequency x, x)) in
	List.sort ~cmp:(fun a b ->  Int.compare (fst a) (fst b)) xs |> List.map ~f:snd

(* ------------------------------------ *)
(* Arithmetic *)

(* 30 *)
let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
    n <> 1 && is_not_divisor 2

(* 31 *)
let rec gcd a b = 
	if b = 0 then a else gcd b (a mod b);;

(* 32 *)
let coprime a b = gcd a b = 1

(* 33 *)
let phi n = 
	if n = 1 
	then 1
	else List.filter (range 1 (n - 1)) ~f:(coprime n) |> List.length

(* 34 *)
let factors n = 
	let rec aux d acc n = 
		if (n = 1) 
		then acc
		else 
			if (n % d = 0)
			then aux d (d::acc) (n / d)
			else aux (d + 1) acc n
	in if (n > 1) then (List.rev (aux 2 [] n)) else [1]

(* 35 *)
let factors2 n = encode (factors n) |> List.map ~f:(fun (a, b) -> (b, a))

(* 36 *)
let phi_improved n = 
	let f k m = (k - 1) * (Int.pow k (m - 1))
	in List.fold (factors2 n) ~init:1 ~f:(fun a (b, m) -> a * f b m)

(* 37 *)

let time f x =
    let start = Unix.gettimeofday ()
    in let res = f x
    in let stop = Unix.gettimeofday ()
    in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)
    in res

let phi_estimate n = 
	let t1 = time phi n in
	let t2 = time phi_improved n in
	t1 - t2

(* 38 *)

let all_primes m n = range m n |> List.filter ~f:is_prime

(* 39 *)

let goldbach n = 
	if n = 2 
	then (1, 1)
	else 
		List.find_exn ~f:(fun (a, b) -> a + b = n) 
			(let xs = all_primes 1 n in List.bind xs (fun x -> List.map ~f:(fun a -> (a, x)) xs))

(* 40 *)
let goldbach_list m n = 
	range m n 
	|> List.filter ~f:(fun x -> x % 2 = 0) 
	|> List.map ~f:(fun x -> printf "%d"  x; (x, goldbach x))

(* 41 *)
type bool_expr =
	| Var of string	
	| Not of bool_expr
	| And of bool_expr * bool_expr
	| Or  of bool_expr * bool_expr

let rec run_bool_expr vars = function
	| Var s -> Map.find vars s
	| Not a -> Option.map ~f:not (run_bool_expr vars a)
	| And (a, b) -> Option.map2 ~f:(&&) (run_bool_expr vars a) (run_bool_expr vars b)
	| Or  (a, b) -> Option.map2 ~f:(||) (run_bool_expr vars a) (run_bool_expr vars b)

let table2 var1 var2 expr = 
	let bool_vals = [true; false] in
	let v2 = List.bind bool_vals (fun x -> List.map ~f:(fun y -> x, y) bool_vals) in
	List.map v2 ~f:(fun (a, b) -> 
		let vars = String.Map.of_alist_exn [var1, a; var2, b] in 
		a, b, Option.value_exn (run_bool_expr vars expr)
	)

(* 42 *)
let rec table_args = function
	| [] -> []
	| [_] -> [[true]; [false]]
	| _::t -> 
		let rest = table_args t in
		List.bind [true; false] (fun x -> List.map ~f:(fun y -> x :: y) rest)

let table names expr = 
	List.map (table_args names) ~f:(fun xs -> 
		let args = List.zip_exn names xs in
		let vars = String.Map.of_alist_exn args in
		args, Option.value_exn (run_bool_expr vars expr)
	)

(* 43 *)
let gray n = 
	let add_byte b rest = List.map ~f:(fun x -> b ^ x) rest in
	let rec aux acc = function
		| 1 -> acc
		| n -> aux (List.append (add_byte "0" acc) (add_byte "1" (List.rev acc))) (n - 1)
	in aux ["0"; "1"] n

(* 44: skipped huffman code *)

(* 45 *)

type 'a binary_tree = 
	| Empty
	| Node of 'a * 'a binary_tree * 'a binary_tree

let rec cbal_tree = function
	| 0 -> [Empty]
	| 1 -> [Node ('x', Empty, Empty)]
	| n -> 
		let m1 = (n - 1) / 2 in
		let m2 = (n - 1 - m1) in
		let nodes ma mb = 
			List.bind ma (fun x -> 
			List.map mb ~f:(fun y -> 
				Node ('x', x, y)))		
		in 
		if m1 = m2 
		then 
			let mx = cbal_tree m1 in
			nodes mx mx
		else
			let mx = cbal_tree m1 in
			let my = cbal_tree m2 in
			List.append (nodes mx my) (nodes my mx)

(* 46 *)
let rec is_equal_structure a b = match a, b with
	| Empty, Empty -> true
	| Node _, Empty | Empty, Node _ -> false
	| Node (_, a1, b1), Node (_, a2, b2) -> is_equal_structure a1 a2 && is_equal_structure b1 b2

let rec rev_structure = function
	| Empty -> Empty
	| Node (v, a, b) -> Node (v, rev_structure b, rev_structure a)

let is_symmetric a = is_equal_structure a (rev_structure a)

(* 47 *)
let construct ns = 
	let rec insert tree n = match tree with
		| Empty -> Node (n, Empty, Empty)
		| Node (m, a, b) -> 
			if m = n
			then Node (m, a, b)
			else if (n < m) 
					then Node (m, insert a n, b)
					else Node (m, a, insert b n)
	in List.fold ~init:Empty ~f:insert ns
