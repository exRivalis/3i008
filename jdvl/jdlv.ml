(*load "graphics.ma"*)
(*type cellule V:vivnte, M:morte*)
type cell = |V
			|M;;

(* grille n*n contenant des cellules *)
type grille = cell array array;;

(* inititialisation de la grille *)
(* renvoie une matrice n*n initialisant tout les champs a cell *)
let init_gen = function n ->
	let c : cell = V in
	Array.make_matrix n n c;;

let gr = init_gen 5;;

gr;;

(* detecte si cel (i, j) existe et renvoie 1 si Vivante *)
let detect gr i j =
	let n = Array.length gr in
	match (i, j) with
	|(i, j) when i<0 || i>=n || j<0 || j>=n -> 0
	|(i, j) -> if gr.(i).(j) == V then 1 else 0;;

(* compte le nombre de voisins vivants de la cell (i, j) *)
let neighbours gr i j = 
	let somme = ref 0 in
		for x=0 to 2 do
			for y=0 to 2 do
				match (x-1, y-1) with
				|(0, 0) -> ()
				|(x, y) -> somme := !somme + (detect gr (i+x) (j+y))
			done
		done;
		!somme;;
(* := affectation de références, ! déréférencement *)

(* retourne letat de la cellule en fonction de ses voisines *)
let next_cell gr i j = 
	match(neighbours gr i j, gr.(i).(j)) with
	|(3, M) -> V
	|(0, V) -> M
	|(1, V) -> M
	|(x, V) when x>3 || x<2 -> M
	|(_, x) -> x;;

(* retourne le nouvelle generation de lal grille *)
let next_gen = function gr ->
	let size = Array.length gr in
	for i=0 to size do
		for j = 0 to size do
			let c = ref gr.(i).(j) in
				c := next_cell gr i j
		done
	done;
	gr;;

(* afficjage en mode graphique en utilisant le module Graphics *)
(* use with ocamlopt -o graphics.ma jdlv jdlv.ml *)
let init_graph gr:
	open Graphics;;
	Graphics.open_graph " 200x200";;
