open Deck
open Player
open Rules
open Seats

type t = {
  players : Seats.t;
  deck : Deck.t;
  dealer_hand : Deck.card list;
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

let empty_table = 
  {players = Seats.empty_seats; deck = Deck.init_deck "Main Deck" 1; dealer_hand = []}

let rep_ok t = failwith "Unimplemented"

let player_names t = 
  Seats.names t.players

let rec get_player players p_name = match players with
  | [] -> failwith "Invalid Name"
  | h::t -> if Player.player_name h = p_name then h else get_player t p_name

let rec swap_player players old_p new_p = match players with
  | [] -> []
  | h::t -> if h = old_p then [new_p] @ t else 
      [h] @ swap_player t old_p new_p

let add_player t p_name p_bankroll = 
  let p = Player.init_player p_name p_bankroll in
  {t with players = Seats.sit_player p t.players}

let remove_player t p_name = 
  {t with players = Seats.rem_player p_name t.players}

let change_player_name t p_name new_name = 
  let p = Seats.get_pl p_name t.players in
  let p' = Player.change_n new_name p in
  {t with players = Seats.update_player p p' t.players}

let deal_card_to_player t p_name = 
  let dealt = Deck.deal_cards 1 t.deck in
  let p = Seats.get_pl p_name t.players in
  {t with players = Seats.deal_cards_to_pl (fst dealt) p t.players; deck = snd dealt}

let deal_card_to_player_spl t p_name =
  let dealt = Deck.deal_cards 1 t.deck in
  let p = Seats.get_pl p_name t.players in
  {t with players = Seats.deal_cards_to_pl_s (fst dealt) p t.players; deck = snd dealt}

let deal_card_to_dealer t =
  let dealt = Deck.deal_cards 1 t.deck in
  {t with deck = snd dealt; dealer_hand = t.dealer_hand @ fst dealt}

let clear_hands t = 
  let f p = Player.clear_hand p in
  {t with players = Seats.map f t.players; dealer_hand = []}

let player_hand t p_name =
  Seats.get_pl p_name t.players |> Player.curr_hand

let player_spl_hand t p_name = 
  match Seats.get_pl p_name t.players |> Player.spl_hand with
  | None -> []
  | Some v -> v

let dealer_hand t =
  t.dealer_hand

let player_hand_value t p_name = 
  player_hand t p_name |> Rules.hand_val

let player_bankroll t p_name = 
  Seats.get_pl p_name t.players |> Player.curr_bankroll

let change_player_bankroll t p_name change_amt = 
  let p = Seats.get_pl p_name t.players in
  let p' = Player.change_bankroll change_amt p in
  {t with players = Seats.update_player p p' t.players}

let eval_hand t = 
  let f p = match Player.spl_hand p with
    | None -> Player.curr_hand p |> Rules.get_result t.dealer_hand |> Player.eval_bet p
    | Some v -> Player.eval_bet_with_split p 
                  (Rules.get_result t.dealer_hand (Player.curr_hand p))
                  (Rules.get_result t.dealer_hand v) in
  {t with players = Seats.map f t.players}

let player_bet n b t = 
  let p = Seats.get_pl n t.players in
  let p' = Player.make_bet b p in
  {t with players = Seats.update_player p p' t.players}

let iter_players f t = 
  let f' p = 
    let new_t = f (Player.player_name p) t in 
    Seats.get_pl (Player.player_name p) new_t.players in
  {t with players = Seats.map f' t.players}

let bust h = Rules.hand_val h |> bust

let run_spl_for_pl p_name t =
  let p = Seats.get_pl p_name t.players in
  let t' = {t with players = Seats.update_player p (Player.initial_split p) t.players} in
  let dealt_first = deal_card_to_player t' p_name in
  let () = player_hand dealt_first p_name |> print_cards "Hand One"  in
  let () = player_spl_hand dealt_first p_name |> print_cards "Hand Two" in
  let rec f tab main = 
    let hand = if main then player_hand tab p_name else player_spl_hand tab p_name in
    let () = print_cards "Hand" hand in
    if not (bust hand) then
      let () = ANSITerminal.(print_string [blue] ("\n" ^ p_name ^ ": Please choose an option." ^ "\n")) in 
      let () = print_options ["Hit"; "Stick"] in let () = print_string "> " in begin
        match main with 
        | true -> begin match Command.parse_hand_option (read_line()) with
            | Hit -> f (deal_card_to_player tab p_name) main
            | Stick -> tab
            |  _-> let () = print_endline "Invalid Option" in f tab main end
        | false -> begin match Command.parse_hand_option (read_line()) with
            | Hit -> f (deal_card_to_player_spl tab p_name) main
            | Stick -> tab 
            | _ -> let () = print_endline "Invalid Option" in f tab main end
      end else let () = ANSITerminal.(print_string [black] "\nBUST\n") in tab in
  let run_first = f dealt_first true in
  f (deal_card_to_player_spl run_first p_name) false


let rec run_hand_for_player p_name t =
  let p_hand = player_hand t p_name in
  match p_hand|> Rules.hand_val |> Rules.bust with
  | true ->
    let () = print_cards "Player's cards" p_hand in 
    let () = ANSITerminal.(print_string [red] "\nBUST\n") in t

  | false -> 
    if p_hand |> Rules.is_blackjack then
      let () = print_cards "Player's cards" p_hand in
      let () = ANSITerminal.(print_string [red] "\nBLACKJACK!\n") in t
    else
      let () = print_cards "Dealer's card" [List.hd t.dealer_hand] in
      let () = print_cards "CARDS" p_hand in
      let () = ANSITerminal.(print_string [blue] ("\n" ^ p_name ^ ": Please choose an option." ^ "\n")) in
      let () = print_options ["Hit"; "Stick"; "DoubleDown"; "Split"] in
      let () = print_string "> " in
      match Command.parse_hand_option (read_line()) with
      | Hit -> deal_card_to_player t p_name |> run_hand_for_player p_name
      | Stick -> t
      | DoubleDown -> let b = Player.curr_bet (Seats.get_pl p_name t.players) in
        if player_bankroll t p_name >= b 
        then let updb = change_player_bankroll t p_name b in 
          let t' = deal_card_to_player updb p_name in
          let () = print_cards "Player's cards" (player_hand t' p_name) in t'
        else let () = print_endline "Insufficient Funds" in run_hand_for_player p_name t
      | Split -> begin
          let b = Player.curr_bet (Seats.get_pl p_name t.players) in
          match player_bankroll t p_name >= b  with
          | true -> begin match Seats.get_pl p_name t.players |> Player.cards_match with
              | true -> run_spl_for_pl p_name t
              | false -> let () = print_endline "Invalid Move" in run_hand_for_player p_name t end
          | false -> let () = print_endline "Insufficient Funds" in run_hand_for_player p_name t
        end

let run_hand_for_players t = 
  let n = Seats.names t.players |> List.filter (fun x -> not (x = "Empty")) in
  let rec print_all names = match names with
    | [] -> t
    | hd::tl -> 
      let () = print_cards hd (player_hand t hd) in print_all tl in

  let rec f names table = match names with
    | [] -> table
    | hd::tl -> run_hand_for_player hd table |> f tl in
  print_all n |> f n

let rec run_bet_for_player p_name t =
  let () = ANSITerminal.(print_string [blue] ("\n" ^ p_name ^ ": Please enter bet.\n")) in
  let () = print_string "> " in
  let bet = read_float_opt() |> Command.parse_bet in
  if bet <= (Seats.get_pl p_name t.players |> Player.curr_bankroll) 
  then player_bet p_name bet t
  else let () = ANSITerminal.(print_string [red] "\nInsufficient funds for bet.\n") in
    run_bet_for_player p_name t

let run_bet_for_players t =
  let n = Seats.names t.players |> List.filter (fun x -> not (x = "Empty")) in
  let rec f names table = match names with
    | [] -> table
    | hd::tl -> run_bet_for_player hd table |> f tl in
  f n t

let deal_new_hand t = 
  let t' = clear_hands t in
  let n = Seats.names t.players |> List.filter (fun x -> not (x = "Empty")) in

  let rec f names table = match names with
    | [] -> table
    | hd::tl -> 
      let x = deal_card_to_player (deal_card_to_player table hd) hd in
      f tl x in
  f n t' |> deal_card_to_dealer |> deal_card_to_dealer

let to_string t =
  let seats = t.players in
  if Seats.num_pls(seats) = 0 then "No players at table" else
    let rec inner ps = match ps with
      | [] -> ""
      | h::t -> if not(h="Empty") then Player.to_string((Seats.get_pl h seats)) ^ "\n\n" 
                                       ^ (inner t) else inner t
    in inner (Seats.names(seats))

let get_table_stats t =
  let number_of_players = Seats.num_pls(t.players) in
  let number_of_decks = Deck.get_num_decks(t.deck) in
  "Number of players: " ^ string_of_int number_of_players ^ " \n" ^
  "Number of decks: " ^ string_of_int number_of_decks

let change_deck_size t n =
  {t with deck = Deck.(init_deck "New Deck" n)}