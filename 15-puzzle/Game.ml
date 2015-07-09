
open Core.Std

type move = Up | Down | Left | Right

let all_moves = [Up; Down; Left; Right]

type label = Label of int

let empty_label = Label 15

type pos = Pos of int * int

let pos_of_int n = Pos (n / 4, n % 4)
let int_of_pos (Pos (a, b)) = a * 4 + b

type vec = Vec of int * int

type board = Board of label array

type game = 
	{ empty_field : pos ;
	  game_board  : board;
	  score       : int }

let get_score game = game.score

(*****************************************)
(* print board *)

let space  = "\t"
let column_numbers = [0; 1; 2; 3]

let line = "+----+----+----+----+\n"

let cell x = 
	if x = "0" 
	then "    "
	else if (String.length x = 1)
		 then "  " ^ x ^ " "
		 else " "  ^ x ^ " "

let show_label (Label n) = 
	let n' = match n with
		| 15 -> 0
		| n  -> n + 1			
	in 
	cell (string_of_int n')	

let post board id = show_label board.(int_of_pos id)

let nums board xs = List.map xs ~f:(post board)
	|> List.fold ~init:""  ~f:(fun a b -> a ^ "|" ^ b)
	|> (fun x ->  space ^ x ^ "|\n")

let column (Board board) i = List.map column_numbers ~f:(fun x -> Pos (i, x)) |> (nums board)

let show_board game = "\n" ^
	(List.fold (List.map column_numbers ~f:(column game.game_board)) 
		~init:"\n"
		~f:(fun res a -> res ^ space ^ line ^ a)
	) ^ space ^ line ^  "\n"

let init_game = {
	empty_field = Pos (3, 3);
	game_board  = Board (Array.init 16 (fun i -> (Label i)));
	score       = 0
}

(*****************************************)

let is_game_over n = (n.game_board = init_game.game_board)

let orient move =
	let (a, b) = match move with
		| Up      -> (-1, 0)
		| Down    -> (1 , 0)
		| Left    -> (0 ,-1)
		| Right   -> (0 , 1)
	in Vec (a, b)


let within (Pos (a, b)) = 
	let p x = x >= 0 && x <= 3 in
	p a && p b

let shift (Vec (va, vb)) (Pos (pa, pb)) = Pos (va + pa, vb + pb)

let switch_array_values a b array = 
	let na = array.(a) in
	let nb = array.(b) in
	array.(b) <- na;
	array.(a) <- nb
	
let move m game = 
	let next_field = shift (orient m) game.empty_field in	
	let switch_fields a b (Board array) = 
		let new_array = Array.copy array in
		let () = switch_array_values (int_of_pos a) (int_of_pos b) new_array in
		Board new_array
	in 
	if (within next_field) 
	then { empty_field = next_field
		 ; game_board = switch_fields next_field game.empty_field game.game_board
		 ; score = game.score + 1 }
	else game

let next_moves game = 
	let move_empty_to v = shift v (game.empty_field) in
	List.filter all_moves ~f:(fun x -> within (move_empty_to (orient x)))

(****************************)
(* shuffle *)


let random_element list = List.nth_exn list (Random.int (List.length list))

let shuffle1 game = move (random_element (next_moves game)) game

let rec shuffleN n game =
	if n = 0
	then game
	else shuffleN (n - 1) (shuffle1 game)

let set_zero_score game = { game with score = 0 }

let shuffle n = set_zero_score (shuffleN n init_game)
