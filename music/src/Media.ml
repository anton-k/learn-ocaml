open Track
open Score

let test () = printf "%s\n" (render (loop 2 (
	chord [
		line [a; b; c]; 
		stretch 10.0 (instr 1 d |> higher 2)])))

