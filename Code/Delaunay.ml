type point = {x: float; y:float}

type triangle = {p1:point; p2:point; p3: point}

type point_set = point list

type triangle_set = triangle list

let rec random (num : int) (max_x : int) (max_y : int) :(point list) =  
	match num with
		|0 -> []
		|i -> {x = float_of_int (Random.int(max_x)); y = float_of_int(Random.int(max_y))}::(random (i-1) max_x max_y)
(*this function return the determinant of 2 vectors v1 and v2 their type is point*)
let determinant2  m =
	m.(0).(0) *. m.(1).(1) -. m.(0).(1) *. m.(1).(0)
(*return the vector whose two extremities are p1 and p2*)
let make_vect p1 p2 =
	{x= p2.x -. p1.x; y =p2.y -. p1.y };;
(*return true if three points are direct false if not*)
let ccw p1 p2 p3 =
	let v1 = make_vect p1 p2 in
	let v2 = make_vect p2 p3 in
	determinant2 [|[|v1.x;v2.y|];[|v1.y;v2.y|]|] >= 0.

let rec determinant n m =
	if n = 2 then 
		determinant2 m
	else
		begin
		let l = Array.length m in
		let pivot =  m.(0).(0) in
		for col = 1 to (l-1) do
			let ratio = m.(0).(col) /. pivot in
			for line = 0 to (l-1) do
				m.(col).(line) <- m.(col).(line) -. m.(0).(line) *. ratio 
			done
		done;
		let new_m = Array.init (l-1) (fun x -> Array.init (l-1) (fun y -> m.(x+1).(y+1))) in
		determinant (n-1) m
		end 







