open Command
open Player
open Deck

type gamestate = TableIdle | HandRunning

type t= {
  st : gamestate;
  table : Table.t;
}

let rec print_options lst = match lst with
  | [] -> ()
  | h::t -> let () = print_endline h in print_options t

let print_cards n lst =
  let rec f x = match x with
    | [] -> ()
    | h::t -> 
      let out = Card.card_n h ^ " of " ^ Card.card_suit h in
      let () = print_endline out in f t in

  let () = ANSITerminal.(print_string [blue] ("\n" ^ n ^ "\n")) in 
  f lst

let run_idle t = 
  let () = ANSITerminal.(print_string [blue] "\nPlease choose an option.\n") in
  let () = print_options ["AddPlayer [name] [bankroll]"; "RemovePlayer [name]"; 
                          "DealHand"; "ChangeDeckSize [num_decks]"; "PrintTableStats"; "PrintPlayers"; 
                          "Bankroll [name]"; "Quit"] in
  print_string  "> ";
  let cmd = parse_game_option (read_line()) in match cmd with
  | AddPlayer (n,b) -> 
    let updated_table = Table.add_player t.table n b in
    {st = TableIdle; table = updated_table}

  | RemovePlayer v -> 
    let updated_table = Table.remove_player t.table v in
    {st = TableIdle; table = updated_table}

  | DealHand -> {st = HandRunning; table = t.table}
  | ChangeDeckSize n -> let () = ANSITerminal.(print_string [blue] 
                                                 ("Successfully changed the table to have " ^ (string_of_int n) ^ " decks.\n")) in 
    let updated_table = Table.change_deck_size t.table n in
    {t with table = updated_table}
  | PrintPlayers -> let () = ANSITerminal.(print_string [blue] (Table.to_string t.table)) in 
    {st = TableIdle; table = t.table}
  | PrintTableStats -> let () = ANSITerminal.(print_string [blue] 
                                                (Table.get_table_stats t.table)) in 
    {st = TableIdle; table = t.table}

  | Bankroll n -> 
    let () = print_endline (n ^ "'s bankroll: $" ^ (Table.player_bankroll
                                                      t.table n |> string_of_float )) in t

  | Quit -> exit 0

let rec run_hand_for_dealer table =
  let () = print_cards "Dealer's Cards: " (Table.dealer_hand table) in
  match Table.dealer_hand table |> Rules.hand_val |> Rules.d_fin with
  | true -> table
  | false -> Table.deal_card_to_dealer table |> run_hand_for_dealer

let run_hand t = 
  let t' = Table.run_bet_for_players t.table |> Table.deal_new_hand
           |> Table.run_hand_for_players |> run_hand_for_dealer in
  let () = ANSITerminal.(print_string [blue] "\nRESULTS\n") in
  let t'' = Table.eval_hand t' |> Table.clear_hands in
  {st = TableIdle; table = t''}

let rec game_thread t = match t.st with
  | TableIdle -> run_idle t |> game_thread
  | HandRunning -> run_hand t |> game_thread

(** [main ()] welcomes the player to the game, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Blackjack.\n");
  game_thread {st = TableIdle; table = Table.empty_table} 

(* Execute the game engine. *)
let () = main ()