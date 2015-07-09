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

type score = note Track.t

(* notes *)

val note : int -> score

val c: score
val cs: score
val d: score
val ds: score

val e: score
val f: score
val fs: score
val g: score
val gs: score

val a: score
val as_: score
val b: score
val es: score

val bs: score
val cf: score
val df: score
val ef: score

val ff: score
val gf: score
val af: score
val bf: score

(* Octaves *)

val higher 	: int -> score -> score
val lower  	: int -> score -> score

val high   	: score -> score
val low    	: score -> score

(* Time *)

val bn 		: score -> score
val hn 		: score -> score
val qn 		: score -> score
val en 		: score -> score
val sn 		: score -> score

(* Rest *)

val bnr 	: score
val wnr 	: score
val hnr 	: score
val qnr 	: score
val enr 	: score
val snr 	: score

(* Volume *)

val louder 	: float -> score -> score
val quieter : float -> score -> score

(* Instrument *)

val instr	: int -> score -> score
val drum	: int -> score -> score
val bam     : float -> score

(* Params *)

val params	: param list -> score -> score

(* Render to Csound Score *)

val render	: score -> string
