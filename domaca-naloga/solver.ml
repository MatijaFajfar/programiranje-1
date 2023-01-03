type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid; possibilities : int list Model.grid }

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let int_list_to_string_fancy lst =
  "[" ^ ( List.fold_left( fun acc x -> acc ^ string_of_int x ^ ";" ) "" lst) ^ "]"


let add_to_grid i j n grid = 
  Array.init 9 (fun a -> Array.init 9 (fun b -> if a = i && b = j then Some n else grid.(a).(b)))


let problemcek = Model.problemcek

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let update_possibilities i j n sudoku = 
  (* V sudoku doda n kot edino možnost v celici na (i, j)-tem mestu ter iz celic v vrstici, stolpcu in škatli odstrani n kot možnost*)
  sudoku.(i).(j) <- [];
  for k = 0 to 8 do
    (*Odstrani iz vrstice in stolpca*) 
    if k != i then sudoku.(k).(j) <- List.filter (fun a -> if a = n then false else true) sudoku.(k).(j);
    if k != j then sudoku.(i).(k) <- List.filter (fun a -> if a = n then false else true) sudoku.(i).(k);
    (*Odstrani iz box-a*)
    if i != (3 * (i / 3) + (k mod 3)) || j != (3 * (j / 3) + (k / 3)) then 
      sudoku.(3 * (i / 3) + (k mod 3)).(3 * (j / 3) + (k / 3)) <-  
        List.filter (fun a -> if a = n then false else true) sudoku.(3 * (i / 3) + (k mod 3)).(3 * (j / 3) + (k / 3))
  done

let new_possibilities (problem : int option Model.grid) =
  let vse_mozne = Array.init 9 (fun i -> Array.init 9 (fun j -> (List.init 9 (fun a -> a+1)))) 
  (*Naredi sudoku kjer so v vsaki celici vse možnosti (tipa available grid)*)
  in 
  for i = 0 to 8 do
    for j = 0 to 8 do 
    if problem.(i).(j) != None then update_possibilities i j (Option.get problem.(i).(j)) vse_mozne
    done 
  done;
  vse_mozne

let initialize_state (problem : Model.problem) : state =
  (* Model.print_grid (int_list_to_string_fancy) (new_possibilities problem.initial_grid); *)
  { current_grid = Model.copy_grid problem.initial_grid; problem ; possibilities = new_possibilities problem.initial_grid}

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = state.current_grid in
    if Model.is_valid_solution (state.problem.initial_grid) (state.current_grid) then Solved (Model.map_grid Option.get solution) 
      else Fail state 

let rec print_list = function 
  [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l

let prvi = function
  |[] -> failwith "Prekratek seznam"
  |x :: xs -> x

let najdi_hipotezo moznosti =
  (* print_string "iscemo hipotezo "; *)
  let a, b, min = ref 9, ref 9, ref 9 in
  for i = 0 to 8 do 
    for j = 0 to 8 do 
      if (moznosti.(i).(j) != []) && (List.length moznosti.(i).(j) < !min) then begin a := i; b:= j; min := (List.length moznosti.(i).(j)) end(*print_int !a; print_int !b;*)
    done
  done;
  !a, !b

  let branch_state (state : state) : (state * state) option =
      (* print_string "branchamo "; *)
    (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
    se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
    let i, j = najdi_hipotezo state.possibilities in
    if not (i = 9) then begin
      let n = prvi (state.possibilities.(i).(j)) in
      let stare_moznosti = Model.copy_grid state.possibilities in
      let nove_moznosti = Model.copy_grid state.possibilities in 
      let nov_grid = add_to_grid i j n state.current_grid in
      update_possibilities i j n nove_moznosti;
      stare_moznosti.(i).(j) <- List.filter ((!=) n) state.possibilities.(i).(j);
      Some ({problem = state.problem; current_grid = nov_grid; possibilities = nove_moznosti},
       {problem = state.problem; current_grid = Model.copy_grid state.current_grid; possibilities = stare_moznosti}) end
    else None

let rec solve_state (state : state) =
  (* print_string "rešujemo "; *)
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  let state = ref state in
  for i = 0 to 8 do 
    for j = 0 to 8 do 
      if List.length !state.possibilities.(i).(j) = 1 then
       state := {problem = !state.problem; current_grid = (add_to_grid i j (prvi !state.possibilities.(i).(j) ) !state.current_grid); possibilities = (new_possibilities (add_to_grid i j (prvi !state.possibilities.(i).(j) ) !state.current_grid))};
    done
  done;
  (* print_state !state; *)
  let state = !state in
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail -> print_string "fail";
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
    (* print_string "raziskujemo "; *)
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None -> 
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
        None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None -> 
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state |> (fun array -> match array with |None -> None |Some array -> Some (Model.map_grid (fun b -> Some b) array))
