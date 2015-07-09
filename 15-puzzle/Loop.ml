open Core.Std
open Game

type query = 
	| Quit 
	| NewGame of int 
	| Play of move

let show_game game = printf "%s" (show_board game)

let remind_moves () = 
	let talk = [
		"You can move an empty field:";
		"  left   or l";
		"  right  or r";
		"  up     or u";
		"  down   or d" ;
        "other options:";
        "  new int  or n int  -- starts a new game, int is a number of shuffles";
        "  quit     or q      -- quits the game";
	]
	in List.iter talk ~f:(printf "%s\n") 

let greetings () = 
	printf "15 puzzle";
	show_game init_game;
	remind_moves ()

let parse_int a = try Some (Int.of_string a) with _ -> None

let rec setup () = 
	printf "\nLet's start a new game!\n";
	printf "Define a complexity (positive integer): ";
	match parse_int (read_line ()) with
	| Some n  -> shuffle n
	| None    -> setup ()

let show_results game = 
	show_game init_game;
	printf "Victory!\n";
	match get_score game = 1 with
	| true  -> printf "You have solved a puzzle in a single step\n"
	| false -> printf "You have solved a puzzle in %d steps\n" (get_score game)

let wrong_move () = printf "Uknown move\n"
let show_ask   () = printf "Next move: "
let quit 	   () = printf "See you next time!\n"

let parse_query x = 
	match String.lowercase (String.strip x) with
	| "up"    | "u" -> Some (Play Up)
	| "down"  | "d" -> Some (Play Down)
	| "right" | "r" -> Some (Play Right)
	| "left"  | "l" -> Some (Play Left)
	| "quit"  | "q" -> Some Quit
	| x when (String.length x > 4) && (String.sub x 0 4 = "new ") -> Option.map (parse_int (String.sub x 4 (String.length x - 4))) ~f:(fun a -> NewGame a)
	| x when (String.length x > 2) && (String.sub x 0 2 = "n ")   -> Option.map (parse_int (String.sub x 2 (String.length x - 2))) ~f:(fun a -> NewGame a)
	| _ -> None

let rec ask_for_move n = 
	show_ask ();
	match parse_query (read_line ()) with
	| Some q -> q
	| None  when n < 2  -> wrong_move (); ask_for_move (n + 1)
	| None 				-> wrong_move (); remind_moves (); ask_for_move (n + 1)

let rec game_loop game = 
	match is_game_over game with
	| true -> show_results game; game_loop (setup ())
	| false -> show_game game; react_on_move game (ask_for_move 0)	
and react_on_move game query = 
	match query with
	| Quit      -> quit ()
	| NewGame n -> game_loop (shuffle n)
	| Play    m -> game_loop (move m game)

let play () = 
	greetings ();	
	game_loop (setup ())

let () = play ()
