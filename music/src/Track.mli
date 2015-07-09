module type Event_type = sig 
	type 'a t = 
	{ start   : float
	; dur     : float
	; content : 'a }

	val dur 	: 'a t -> float
	val delay 	: float -> 'a t -> 'a t
	val stretch	: float -> 'a t -> 'a t

	val map 	: 'a t -> f:('a -> 'b) -> 'b t

	val temp    : 'a -> 'a t
end

module Event : Event_type

(* The main type for the Track *)
type 'a t =
	{ dur     : float
	; events  : 'a Event.t list }

val temp : 'a -> 'a t
val rest : float -> 'a t
val map  : 'a t -> f:('a -> 'b) -> 'b t

val dur   	: 'a t -> float
val delay 	: float -> 'a t -> 'a t
val stretch	: float -> 'a t -> 'a t

val (=:=)   : 'a t -> 'a t -> 'a t
val (+:+)   : 'a t -> 'a t -> 'a t
val line    : 'a t list -> 'a t
val chord   : 'a t list -> 'a t
val loop    : int -> 'a t -> 'a t
