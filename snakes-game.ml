let select_tail l = 
	match l with
	| [] -> []
	| _::tail -> tail

let rec reverse_list l =
	match l with
	| [] -> []
	| head::tail -> (reverse_list tail) @ [head]

let display_location (x,y) = 
	print_endline (string_of_int x ^ ";" ^ string_of_int y);;

let rec is_element (x,y) l =
	match l with
	| [] -> false
	| (k,v)::tail -> if x=k && y=v then true else is_element (x,y) tail

let rec lookupx x l =
	match l with
	| [] -> false
	| (k,_)::tail -> if k=x then true else lookupx x tail

let rec lookupy y l =
	match l with
	| [] -> false
	| (_,v)::tail -> if v=y then true else lookupy y tail

let rec printgrid2 gridlength snake (mx,my) h i =
	if i = gridlength then print_string ""
	else if h = 0 then print_string "#_ _ _ _ _ _ _ _ #"
	else if (i = 0 || i = (gridlength-1)) then (print_string "|"; printgrid2 gridlength snake (mx,my) h (i+1))
	else if (lookupy h snake) && (h = my) && (i = mx) && (lookupx i snake) then (print_string "X "; printgrid2 gridlength snake (mx,my) h (i+1))
	else if (lookupy h snake) && (lookupx i snake) then (print_string "S "; printgrid2 gridlength snake (mx,my) h (i+1))
	else if (h = my) && (i = mx) then (print_string "M "; printgrid2 gridlength snake (mx,my) h (i+1))
	else (print_string "  "; printgrid2 gridlength snake (mx,my) h (i+1));;

let rec print_lines2 gridlength snake (mx,my) h i gridheight = 
	if (h = gridheight) then print_endline "#_ _ _ _ _ _ _ _ #"
	else (printgrid2 gridlength snake (mx,my) h i; print_endline ""; print_lines2 gridlength snake (mx,my) (h+1) i gridheight );;

let rec display_snake snake = 
	match snake with
	| [] -> print_endline "-snake"
	| (k,v)::tail -> display_location(k,v); display_snake tail;;

let show_mouse (x,y) = 
	display_location(x,y); 
	print_endline "-mouse";;

let display_board mouse snake wall = 
	print_lines2 (wall+1) snake mouse 0 0 (wall+1);
	show_mouse mouse;
	display_snake snake;;

type direction = DIRECTION_UP | DIRECTION_DOWN | DIRECTION_RIGHT | DIRECTION_LEFT;;

let rec get_direction () =
	let input_direction = read_int() in
	match input_direction with
	| 1 -> DIRECTION_UP
	| 2 -> DIRECTION_DOWN
	| 3 -> DIRECTION_LEFT
	| 4 -> DIRECTION_RIGHT
	| _ -> get_direction();;

let rec create_rat rat = 
	Random.self_init();
	let x = (Random.int 8) + 1 and
	    y = (Random.int 8) + 1 in
	if is_element (x,y) rat then
		create_rat rat
	else
		(x,y);;

let rec create_snake =
	Random.self_init();
	let x = (Random.int 8) + 1 and
	    y = (Random.int 8) + 1 in
	[(x,y)];;

let update_snake_tail = function
	| [] -> []
	| snake -> reverse_list(select_tail(reverse_list snake));;

let is_rat_consumed mouse snake =
	match snake with
	| [] -> false
	| (head::_) -> head = mouse;;

let game_update snake mouse =
	let direction = get_direction() in 
	match is_rat_consumed mouse snake with
	| true -> (match snake with
						 | [] -> (snake,mouse)
						 | ((x,y)::_) -> match direction with
														 | DIRECTION_UP -> let snake2 = (x,y+1)::snake in
														                   let mouse2 = create_rat snake in
																							 (snake2,mouse2)
														 | DIRECTION_DOWN -> let snake2 = (x,y-1)::snake in
														 										 let mouse2 = create_rat snake in
																								 (snake2,mouse2)
														 | DIRECTION_LEFT -> let snake2 = (x-1,y)::snake in
																								 let mouse2 = create_rat snake in
																								 (snake2,mouse2)
														 | DIRECTION_RIGHT -> let snake2 = (x+1,y)::snake in
																									let mouse2 = create_rat snake in
																									(snake2,mouse2)
														 | _ -> (snake,mouse)
							)
	| false -> (match snake with
							| [] -> (snake,mouse)
							| ((x,y)::_) -> match direction with
															| DIRECTION_UP -> let snake2 = (x,y+1)::update_snake_tail(snake) in (snake2,mouse) 
															| DIRECTION_DOWN -> let snake2 = (x,y-1)::update_snake_tail(snake) in (snake2,mouse)
															| DIRECTION_LEFT -> let snake2 = (x-1,y)::update_snake_tail(snake) in (snake2,mouse)
															| DIRECTION_RIGHT -> let snake2 = (x+1,y)::update_snake_tail(snake) in (snake2,mouse)
															| _ -> (snake,mouse)
							);;

let rec hit_wall snake wall =
	match snake with
	| [] -> false
	| (k,v)::tail -> if (k=0 || k=wall || v=0 || v=wall) then true else hit_wall tail wall;;

let rec consumed_itself snake =
	match snake with 
	| [] -> false
	| (k,v)::tail -> if (is_element (k,v) tail) then true else consumed_itself tail

let rec call_game snake mouse wall = 
	if (hit_wall snake wall) || (consumed_itself snake) then
		(print_endline "Well Played! Final Score is "; print_int (List.length snake))
	else
		display_board mouse snake wall;
	let (snake2,mouse2) = game_update snake mouse in
		call_game snake2 mouse2 wall;;

let start () =
	let snake = create_snake in
	let rat = create_rat snake in 
	let wall = 9 in
	call_game snake rat wall;;

start();;

		
			
				
					
						
							
								
									
										
											
												
													
														
															
																
																	  
