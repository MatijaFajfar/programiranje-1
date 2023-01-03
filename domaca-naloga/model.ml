(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = ('a Array.t) Array.t (*tabela tabel tipa a*)

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = 
  Array.copy grid.(row_ind)
let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box (grid : 'a grid) (box_ind : int) =
  Array.init 9 (fun ind -> grid.(3 * (box_ind / 3) + (ind / 3)).(3 * (box_ind mod 3) + (ind mod 3)))

let boxes grid = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid =
  Array.init 9 (fun row_ind -> Array.init 9 (fun col_ind -> f (grid.(row_ind).(col_ind))))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid } 

let print_problem problem : unit =
  print_grid (fun (element : int option) ->  match element with | None -> " " | Some element -> (string_of_int element)) problem

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = 
  print_grid (fun a -> match a with |None -> " " |Some k -> string_of_int k ) solution

let is_valid_solution (problem: int option grid) (solution: int option grid) =
  try
    for i = 0 to 8 do
      if not ((List.sort compare (Array.to_list (get_row solution i)) = [Some 1; Some 2; Some 3; Some 4; Some 5; Some 6; Some 7; Some 8; Some 9]) &&
        (List.sort compare (Array.to_list (get_column solution i)) = [Some 1; Some 2; Some 3; Some 4; Some 5; Some 6; Some 7; Some 8; Some 9]) &&
        (List.sort compare (Array.to_list (get_box solution i)) = [Some 1; Some 2; Some 3; Some 4; Some 5; Some 6; Some 7; Some 8; Some 9])) then raise Exit; 
      for j = 0 to 8 do
        if (not ((problem.(i).(j) = None))) && (not (Option.get problem.(i).(j) = Option.get solution.(i).(j))) then raise Exit
      done
    done;
    true
  with Exit -> false

let problemcek = [|
  [|Some 4; Some 8; Some 3; Some 9; Some 2; Some 1; Some 6; Some 5; Some 7|];
  [|Some 9; Some 6; Some 7; Some 3; None  ; Some 5; Some 8; Some 2; Some 1|];
  [|Some 2; Some 5; Some 1; Some 8; Some 7; Some 6; Some 4; Some 9; Some 3|];
  [|Some 5; Some 4; Some 8; Some 1; Some 3; Some 2; Some 9; Some 7; Some 6|];
  [|Some 7; Some 2; Some 9; None  ; Some 6; Some 4; None  ; Some 3; Some 8|];
  [|Some 1; Some 3; Some 6; Some 7; Some 9; Some 8; None  ; Some 4; Some 5|];
  [|Some 3; Some 7; Some 2; Some 6; Some 8; Some 9; Some 5; Some 1; Some 4|];
  [|Some 8; Some 1; Some 4; Some 2; Some 5; Some 3; Some 7; Some 6; Some 9|];
  [|Some 6; Some 9; Some 5; Some 4; Some 1; Some 7; Some 3; Some 8; Some 2|]|]

let resitev = [|
  [|Some 4; Some 8; Some 3; Some 9; Some 2; Some 1; Some 6; Some 5; Some 7|];
  [|Some 9; Some 6; Some 7; Some 3; Some 4; Some 5; Some 8; Some 2; Some 1|];
  [|Some 2; Some 5; Some 1; Some 8; Some 7; Some 6; Some 4; Some 9; Some 3|];
  [|Some 5; Some 4; Some 8; Some 1; Some 3; Some 2; Some 9; Some 7; Some 6|];
  [|Some 7; Some 2; Some 9; Some 5; Some 6; Some 4; Some 1; Some 3; Some 8|];
  [|Some 1; Some 3; Some 6; Some 7; Some 9; Some 8; Some 2; Some 4; Some 5|];
  [|Some 3; Some 7; Some 2; Some 6; Some 8; Some 9; Some 5; Some 1; Some 4|];
  [|Some 8; Some 1; Some 4; Some 2; Some 5; Some 3; Some 7; Some 6; Some 9|];
  [|Some 6; Some 9; Some 5; Some 4; Some 1; Some 7; Some 3; Some 8; Some 2|]|]

(* type mozno =
  | Pravilen of int
  | Mozni of int * (int array)

let update_grid i j n grid =
  for k = 0 to 8 do
    match grid.(i).(k) with 
    | Mozni (a, moznosti) -> (moznosti.(n-1) <- 0)
    | Pravilen a -> ();
    match grid.(k).(j) with 
    | Mozni (a, moznosti) -> (moznosti.(n-1) <- 0)
    | Pravilen a -> ();
    match grid.().() with 
    | Mozni (a, moznosti) -> (moznosti.(n-1) <- 0)
    | Pravilen a -> ()
  done

let nova_moznosti (problem : int option grid) : mozno grid =
  (*Naredi nov grid, kjer so celice ali pravilne ali vsebujejo vse moznosti in stevilo koliko moznosti je*)
  (* let moznosti = Array.init 9 (fun i -> Array.init 9 (fun k -> match problem.(i).(k) with None -> Mozni (9, (Array.init 9 (fun k -> k + 1))) | Some n -> Pravilen n)) in *)
  let moznosti = Array.init 9 (fun i -> Array.init 9 (fun k -> Mozni (9, (Array.init 9 (fun k -> k + 1))))) in
  for i = 0 to 8 do
    for j = 0 to 8 do
      if problem.(i).(j) != None then
        update_grid i j (Option.get problem.(i).(j)) moznosti
    done
  done;
  moznosti *)