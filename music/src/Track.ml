open Core.Std

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

module Event : Event_type = struct
	type 'a t = 
	{ start   : float
	; dur     : float
	; content : 'a }

	let temp x = { start = 0.0; dur = 1.0; content = x }

	let map event ~f:f = { event with content = f event.content }

	let dur event = event.dur
	let delay dt event = { event with start = event.start +. dt }
	let stretch k event = { event with start = k *. event.start; dur = k *. event.dur }
end

(* The main type for the Track *)
type 'a t =
	{ dur     : float
	; events  : 'a Event.t list }

let temp x = { dur = 1.0; events = [Event.temp x] }
let rest dt = { dur = dt; events = [] }

let map track ~f:f = { track with events = List.map track.events (Event.map ~f:f) }

let dur track = track.dur

let delay dt track = {
	dur    	= dt +. track.dur; 
	events 	= List.map track.events (Event.delay dt) }

let stretch k track = {
	dur  	= k *. track.dur;
	events 	= List.map track.events (Event.stretch k)
}

let (=:=) a b = { 
	dur 	= Float.max a.dur b.dur;  
	events 	= List.append a.events b.events }

let (+:+) a b = a =:= (delay (dur a) b)

let withDefaultTrack optionTrack = Option.value optionTrack ~default:(rest 0.0) 

let line  tracks = withDefaultTrack (List.reduce tracks (+:+)) 
let chord tracks = withDefaultTrack (List.reduce tracks (=:=)) 

let rec replicate n a = 
	if n <= 0
	then []
	else a :: replicate (n - 1) a

let loop n track = line (replicate n track)

let append a b = a =:= b
let empty      = rest 0.0
