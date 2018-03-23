let  calcul_prefixe l = 
	let rec prefixe l acc =
		match l with
		|[] -> 0
		|t::x::q when t = x -> prefixe (x::q) (acc+1)
		| _ -> acc
	in prefixe l 1;;
		
		
	
print_int(calcul_prefixe [1; 1; 1; 2; 2; 1]);;
print_newline();;


let l = [1; 1; 1; 2; 2; 1];;

let rec print_list l = match l with
	|[] -> ()
	|e::f -> print_int e; print_string " "; print_list f;; 

let rec delete l x =
	match x with
	| 0 -> l
	|1 -> delete (List.tl l) 0
	|_ -> delete (List.tl l) (x-1);;
	
print_list (delete l 3);;
print_newline();;


let genere_liste l =
	let rec gen l acc =
		match l with 
		|[] -> acc
		| _ -> let n = calcul_prefixe(l)
			in let acc2 = List.concat([acc; n::List.hd(l)::[]])
			in gen (delete l n) acc2
	in gen l [];;

print_list(genere_liste l);;
print_newline();;


let genere k=
	let rec gen lk acc=
		match acc with 
		|0 -> lk
		|_ -> let x = List.concat([lk; genere_liste lk]) in List.concat([x; (gen x (acc-1))]) 
	in gen [k] k;;
print_list(genere 2);;
