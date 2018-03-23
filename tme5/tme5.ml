(* lecture fichier

let open_file path=
	let lines = ref [] in
	let f = open_in path in
	try 
		while true; do
			lines := input_line f :: !lines
		done; !lines  
	with End_of_file -> close_in f;
	List.rev !lines;;

let liste_string = open_file "text.txt";;

*)
(* une marque de formatage*)
type fmark =
	|BOLD
	|IT
	|SUB

(* un caractere formate *)
type fchar = char * fmark list 

(* une ligne de texte formatee *)
type fline = fchar list

let b_bold = ('b', [BOLD])
(* prend un ensemble l de marqueurs et renvoie une liste ou le marqueur a est ajoute *)
let rec set_mark l a = 
	match l with
	|[] -> []
	|(c, fmarks)::tl ->  (c, a::fmarks)::(set_mark tl a)

set_mark [('a', [BOLD])] BOLD
