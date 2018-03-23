type 'a graph = {sommet: 'a array; matrice: bool array array};;
let indice a s = 
	let rec index a s acc = match s with |[] -> failwith "not_found"
										|h::t -> if a == h then acc
												else index a t (acc+1)
												in index a s 0;;
let successeur g a = match g with {sommet=s; matrice=m} ->
	let liste = m.(a) in
	 
	let rec succ l acc = 
		match l with |[] -> acc
					 |h::t -> if h then succ t (List.append acc [(List.length acc +1)]) else succ t acc in succ liste [];;
