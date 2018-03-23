type t = char array array

let get_letter board i j =
  board.(i).(j)

let dim board =
  Array.length board

let all_positions board =
  failwith "Unimplemented"

let is_valid_pos board (i, j) =
  	let n = dim board in
  	match (i, j) with
  	|(a, b) when a >= 0 && a < n && b>=0 && b<n -> true
  	| _ -> false

let are_neighbours board (i, j) (i', j') =
	let v1 = is_valid_pos board (i, j) in
	let v2 = is_valid_pos board (i', j') in
	match (v1, v2) with
	| (true, true) -> (match (i, j) with
						|(i', j') -> false
						| _ -> if (j-1 <= j') && (j' <= j+1) && (i-1 <= i') && (i' <= i+1) then true else false)
	| _ -> false


let neighbours board (i, j) =
  (* constryire la liste des voisins potentiels*)
  let voisins = [(i-1, j-1); (i-1, j); (i-1, j+1);
  					(i, j-1); (i, j); (i, j+1);
					(i+1, j-1); (i+1, j); (i+1, j+1)] in
	let rec f l = match l with
		| [] -> l
		|hd::tl -> if is_valid_pos board hd then hd :: (f tl) else f tl in
	let v = f voisins in
	Iter.to_rev_list v
	(* TODO retourner un iterateur pas une liste *)

let make dim make_char =
  Array.make_matrix dim dim make_char

let from_string s =
  (* failwith "Unimplemented" *)
  (* verifier que la racine carrée de length s est un entier *)
  let n = String.length s in
  let sq = sqrt (float_of_int n) in
  if (sq ** 2.) != (float_of_int n) then None
  else let board = Array.make_matrix (int_of_float sq) (int_of_float sq) 'a' in
  let i = ref 0 and j = ref 0 in
  for k = 0 to n do
  	board.(!i).(!j) <- String.get s k; match !i with | sq -> !i = 0; !j = !j+1
													| _ -> !i = !i +1
	done;
	(* board *)
	None


let print board =
  failwith "Unimplemented"
