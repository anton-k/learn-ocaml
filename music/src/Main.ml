open Core.Std

open Track
open Score

let test () = printf "%s\n" (render (loop 2 (
	chord [
		line [a; b; c]; 
		params [Pstring "Hello"; Pint 1; Pstring "World"; Pfloat 5.2] 
			(stretch 10.0 (instr 1 d |> higher 2))])))

