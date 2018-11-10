

(** [sort_leadeboard leaderboard] sorts leaderboard in descending order 
    by score*)
let sort_leaderboard leaderboard  = 
  List.sort (fun a b -> 
      if ((fst a) > (fst b)) then (-1) else  1) leaderboard

(**[write_csv] represents a csv file in the terminal, given [game_name]
  provided by the user and the [entries] that sets up the leaderboard*)
let write_csv entries game_name =
  let num = 1 in 

  let output_channel = (open_out (game_name)) in 
  let title_name = Str.string_after game_name 
      (String.index game_name '/' + 1)in
  print_string "\n\n";
  print_endline ("\tPacman "^title_name ^  " Leaderboard");
  print_endline "   NAME       SCORE\tLEVELS COMPLETED ";
  print_endline "-----------------------------------------";
  let rec write_csv' num entries =
    if (num > 9 ) then () else 
      match entries with 
      |(score,(name,level))::t -> 
        let display_score = if (score>100) then (string_of_int score) 
          else ((string_of_int score)^"    ")in
        let display_name  = 
          String.sub (name ^"         ") 0 10 in
        print_endline ((string_of_int num) ^". " 
                       ^ (display_name)^" "^display_score^"\t\t"^level);
        Printf.fprintf output_channel "%s\n" ((string_of_int num) ^".,"
                                              ^ (name)^
                                              ", "^
                                              (string_of_int score)^", " 
                                              ^level); 
        write_csv' (num+1) t
      |_ -> close_out output_channel;
  in 
  write_csv' num (sort_leaderboard entries)

(**[addEntry leaderboard] send all scores on leaderboard to be sorted 
   and written by csv*)
let addEntry leaderboard =
  let entries = sort_leaderboard leaderboard in
  write_csv (entries)

(**[process_leaderboard] prints the string asking for the user name that is 
  given a [leaderboard] with [score], [level], and [game_name]*)
let process_leaderboard leaderboard score level game_name= 
  ANSITerminal.(print_string [yellow] 
                  "\nEnter name to be added to the leaderboard! \n> ");
  match read_line () with 
  | exception End_of_file -> ()
  | file -> let name  = file in

    let rec process_leaderboard' entries = function
      |[rank;name;score;level]::t -> 
        process_leaderboard' ((int_of_string score,(name,level))::entries) t
      |_ -> entries
    in let board = process_leaderboard' [] leaderboard in
    write_csv ((score,(name, level))::board) game_name


let load_leaderboard score level game_name= 
  let parsed_name = "leaderboards/" ^ 
                    (Str.string_after game_name 
                       (String.index game_name '/' + 1)) in 
  let leaderboard = try ( Util.arr_to_list
                            (Csv.to_array(Csv.load ( parsed_name))))
    with _ ->  [] in
  process_leaderboard leaderboard  score (string_of_int level) parsed_name
