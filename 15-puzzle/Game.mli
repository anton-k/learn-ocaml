
type move = Up | Down | Left | Right

type pos
type board

type game

val get_score : game -> int
val show_board : game -> string
val init_game  : game
val is_game_over : game -> bool
val move : move -> game -> game

val shuffle : int -> game
