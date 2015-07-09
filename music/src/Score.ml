open Core.Std
open Track

type instr  = int
type volume = float
type pitch	= int
type param = Pint of int | Pfloat of float | Pstring of string

type note = {
	instr	: instr;
	volume	: volume;
	pitch	: pitch;
	is_drum	: bool;
	params  : param list
}

type score = note t

(* notes *)

let note n = temp { instr = 0; volume = 0.5; pitch = 60 + n; is_drum = false; params = [] }

let c   = note 0
let cs  = note 1
let d   = note 2
let ds  = note 3
let e   = note 4
let f   = note 5
let fs  = note 6
let g   = note 7
let gs  = note 8
let a   = note 9
let as_ = note 10
let b   = note 11

let es  = f
let bs  = c

let cf  = b
let df  = cs
let ef  = ds
let ff  = e
let gf  = fs
let af  = gs
let bf  = as_

(* Octaves *)

let higher n t = map t ~f:(fun a -> { a with pitch = a.pitch + 12 * n }  )
let lower  n t = higher (-n) t

let high t = higher 1 t
let low t = lower 1 t

(* Time *)

let bn t = stretch 2.0 t
let hn t = stretch 0.5 t
let qn t = stretch 0.25 t
let en t = stretch 0.125 t
let sn t = stretch 0.0625 t

(* Volume *)

let louder n t = map t ~f:(fun a -> { a with volume = a.volume +. n })
let quieter n t = louder (~-. n) t

(* Instrument *)

let instr n t = map t ~f:(fun a -> { a with instr = n; is_drum = false })
let drum  n t = map t ~f:(fun a -> { a with instr = n; is_drum = true  })

let bam n = temp ({ instr = 0; volume = n; pitch = 0; is_drum = true; params = [] })

(* Rest *)

let bnr = rest 2.0
let wnr = rest 1.0
let hnr = rest 0.5
let qnr = rest 0.25
let enr = rest 0.125
let snr = rest 0.0625

(* Params *)

let params ps t = map t ~f:(fun a -> { a with params = ps })

(*****************************************)
(* Render to Csound Score *)

let frequency_of_midi n = 440.0 *. 2.0 ** (Float.of_int (n - 69) /. 12.0)

let csd_float s = 
	let last_char s = String.get s (String.length s - 1) in
	if last_char s = '.'
	then s ^ "0"
	else s

let string_of_params params = 
	let p = function 
		| Pint n -> string_of_int n
		| Pfloat f -> csd_float (Float.to_string f)
		| Pstring s -> "\"" ^ s ^ "\""
	in
	String.concat ~sep:" " (List.map params ~f:p)

let csd_drum_params e = Pint (e.Event.content.instr) 
	:: Pfloat e.start :: Pfloat e.dur
	:: Pfloat e.content.volume
	:: e.content.params

let csd_note_params e = Pint (e.Event.content.instr) 
	:: Pfloat e.start :: Pfloat e.dur
	:: Pfloat e.content.volume :: Pfloat (frequency_of_midi e.content.pitch) 
	:: e.content.params

let csd_note e = "i" ^ string_of_params (
	if e.Event.content.is_drum
	then csd_drum_params e
	else csd_note_params e )
	
let render score = String.concat ~sep:"\n" (List.map score.events ~f:csd_note)
