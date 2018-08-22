(* Cell format is as follows in file:
 * Index, Winner, Turn, BoardState, nextIndexArray *)

type t = State of string * string * string * string array * int * string array

let diag state tuple =
  let (check,left1,left2,right1,right2,diag1,diag2) = tuple in
  if String.compare state.(check) " " = 0 then
    false
  else if (String.compare state.(check) state.(left1) = 0 &&
           String.compare state.(check) state.(left2) = 0) ||
          (String.compare state.(check) state.(right1) = 0 &&
           String.compare state.(check) state.(right2) = 0) ||
          (String.compare state.(check) state.(diag1) = 0 &&
           String.compare state.(check) state.(diag2) = 0)
  then
    true
  else
    false

let cross state tuple =
  let (check,left,right,fwd1,fwd2) = tuple in
  if String.compare state.(check) " " = 0 then
    false
  else if ((String.compare state.(check) state.(left)) = 0 &&
           (String.compare state.(check) state.(right)) = 0) ||
          ((String.compare state.(check) state.(fwd1)) = 0 &&
           (String.compare state.(check) state.(fwd2)) = 0)
  then
    true
  else
    false

let mid_cell state =
  if String.compare state.(4) " " = 0
  then
    false
  else if (String.compare state.(4) state.(0) = 0 &&
           String.compare state.(4) state.(8) = 0) ||
          (String.compare state.(4) state.(1) = 0 &&
           String.compare state.(4) state.(7) = 0) ||
          (String.compare state.(4) state.(2) = 0 &&
           String.compare state.(4) state.(6) = 0) ||
          (String.compare state.(4) state.(5) = 0 &&
           String.compare state.(4) state.(3) = 0)
  then
    true
  else
    false          

let validate_state state idx =
  match idx with
  | 0 -> diag state (0,1,2,3,6,4,8)
  | 2 -> diag state (2,5,8,1,0,4,6)
  | 8 -> diag state (8,7,6,5,2,4,0)
  | 6 -> diag state (6,3,0,7,8,4,2)
  | 1 -> cross state (1,0,2,4,7)
  | 3 -> cross state (3,0,6,4,5)
  | 5 -> cross state (5,8,2,4,3)
  | 7 -> cross state (7,6,8,4,1)
  | 4 -> mid_cell state
  | _ -> false

let pick_available state =
  let rec aux idx =
    match (idx < 9) with
    | false -> -1
    | true ->
      if String.compare " " state.(idx) = 0 then idx
      else aux (idx+1)
  in
  aux 0

let find_best_move state player =
  let rec aux state_cur idx =
    match (idx < 9) with
    | false -> pick_available state_cur
    | true ->
      if String.compare " " state_cur.(idx) = 0 then
        begin
          let state_copy = Array.copy state_cur in
          state_copy.(idx) <- player;
          if validate_state state_copy idx = true || 
             aux state_copy 0 <> -1
          then
            idx
          else
            -1
        end
      else
        aux state_cur (idx+1)
  in
  aux state 0

let get_ai_move state player =
  let rec aux idx =
    if idx = 9 then -1
    else if String.compare " " state.(idx) = 0 then
      let state_copy = Array.copy state in
      state_copy.(idx) <- player;
      match (validate_state state_copy idx) with
      | true -> idx
      | false -> aux (idx+1)
    else
      aux (idx+1)
  in
  let new_idx = aux 0 in
  if new_idx = -1 then
    find_best_move state player
  else
    new_idx

let print_game s =
  let rec aux idx =
    if idx < 9 then
      begin
        print_string s.(idx);
        if idx mod 3 = 2 then print_endline "";
        aux (idx+1)
      end
  in
  print_endline "";
  aux 0

let genStates() =
  let rec aux state =
    let State(cur_idx,win,turn,cell_state,nextai,keys) = state in
    let keep_keys = Array.copy keys in
    let new_nextai = get_ai_move cell_state turn in
    let new_turn = if String.compare turn "X" = 0 then "O" else "X" in
    let rec inner_loop idx =
      match (idx < 9) with
      | false -> []
      | true ->
        if String.compare " " cell_state.(idx) <> 0 then
          begin
            keep_keys.(idx) <- "";
            inner_loop (idx+1)
          end
        else
          begin
            let new_idx = cur_idx^(string_of_int idx) in
            let state_copy = Array.copy cell_state in
            let keys_copy = Array.copy keys in
            state_copy.(idx) <- new_turn;
            keys_copy.(idx) <- (keys_copy.(idx)^new_idx);
            keep_keys.(idx) <- keys_copy.(idx);
            let winner = if (validate_state state_copy idx) = true then new_turn else " " in
            let ret_state = State(new_idx,winner,new_turn,state_copy,new_nextai,keys_copy) in
            if String.compare " " winner <> 0 then
              ret_state::(inner_loop (idx+1))
            else
              (aux ret_state)@(inner_loop (idx+1))
          end
    in
    (State(cur_idx,win,turn,cell_state,new_nextai,keep_keys))::(inner_loop 0)
  in
  aux (State("0"," ","O",[|" ";" ";" ";" ";" ";" ";" ";" ";" "|],0,
             [|"";"";"";"";"";"";"";"";""|]))

let getStates output =
  let l = genStates() in
  let rec string_states ll =
    match ll with
    | [] -> output_string output ""
    | State(idx,win,turn,cells,nextai,nxtidx)::tl ->
      output_string output (idx^","^win^","^turn
                            ^","^cells.(0)^","^cells.(1)^","^cells.(2)
                            ^","^cells.(3)^","^cells.(4)^","^cells.(5)
                            ^","^cells.(6)^","^cells.(7)^","^cells.(8)
                            ^","^(string_of_int nextai)
                            ^","^nxtidx.(0)^","^nxtidx.(1)^","^nxtidx.(2)
                            ^","^nxtidx.(3)^","^nxtidx.(4)^","^nxtidx.(5)
                            ^","^nxtidx.(6)^","^nxtidx.(7)^","^nxtidx.(8)
                            ^","^"\n");
      string_states tl
  in
  output_string output "index,win,turn,c1,c2,c3,c4,c5,c6,c7,c8,c9,aiNext,s1,s2,s3,s4,s5,s6,s7,s8,s9\n";
  string_states l

let () =
  if (Array.length Sys.argv) = 1 then
    begin
      let new_file = open_out "tttStates.csv" in
      getStates new_file;
      print_endline "tttStates.csv created"
    end
  else
    print_endline "Do not provide any arguments"
