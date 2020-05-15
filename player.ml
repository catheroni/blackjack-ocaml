open Deck
open Rules

module Player = struct

  type card = Deck.card

  type t = {
    name : string;
    bankroll : float;
    curr_hand : (card list);
    split_hand : card list option;
    curr_bet : float;
  }

  exception InsuffFunds

  let init_player n b = 
    {name = n; bankroll = b; curr_hand = []; split_hand = None; curr_bet = 0.0}

  let player_name p = 
    p.name

  let curr_hand p = p.curr_hand

  let spl_hand p = p.split_hand

  let cards_match p =
    match List.length p.curr_hand = 2 with
    | true -> 
      let c1_val = List.hd p.curr_hand |> Card.card_val in
      let c2_val = List.hd (List.tl p.curr_hand) |> Card.card_val in begin
        match c1_val = c2_val with
        | true -> true
        | false -> false end
    | false -> false

  let curr_bankroll p = p.bankroll

  let curr_bet p = p.curr_bet

  let change_n n p =
    {p with name = n}

  let initial_split p =
    let c1 = List.hd p.curr_hand in
    let c2 = List.hd (List.tl p.curr_hand) in
    {p with curr_hand = [c1]; split_hand = Some [c2]}

  let deal_card_to_spl_hand c p = 
    let h = match p.split_hand with
      | None -> [c]
      | Some v -> v @ [c] in
    {p with split_hand = Some h}

  let deal_card_to_hand c p = 
    {p with curr_hand = p.curr_hand @ [c]}

  let clear_hand p = 
    {p with curr_hand = []; split_hand = None}

  let change_bankroll amt p =
    {p with bankroll = p.bankroll +. amt}

  let make_bet amt p = 
    if amt > p.bankroll then raise InsuffFunds
    else {p with curr_bet = p.curr_bet +. amt}

  let eval_bet p res = match res with
    | W -> 
      let () = print_endline (p.name ^ " wins: $" ^ (p.curr_bet |> string_of_float)) in
      {p with bankroll = p.bankroll +. p.curr_bet; curr_bet = 0.0}

    | P -> 
      let () = print_endline (p.name ^ ": Push.") in
      {p with curr_bet = 0.0}
    | WB -> let () = print_endline 
                (p.name ^ " wins with blackjack!: $" ^ (p.curr_bet |> string_of_float)) in
      {p with bankroll = p.bankroll +. (p.curr_bet *. 1.5) ; curr_bet = 0.0}
    | L -> 
      let () = print_endline (p.name ^ " loses: $" ^ (p.curr_bet |> string_of_float)) in
      {p with bankroll = p.bankroll -. p.curr_bet; curr_bet = 0.0}

  let eval_bet_with_split p res1 res2 = 
    let b = p.curr_bet in
    let p' = match res1 with
      | W -> 
        let () = print_endline (p.name ^ " wins: $" ^ (p.curr_bet |> string_of_float)) in
        {p with bankroll = p.bankroll +. b}

      | P -> 
        let () = print_endline (p.name ^ ": Push.") in
        p
      | WB -> let () = print_endline (p.name ^ " wins with blackjack: $" ^ 
                                      (p.curr_bet *. 1.5 |> string_of_float)) in
        {p with bankroll = p.bankroll +. p.curr_bet; curr_bet = 0.0}
      | L -> let () = print_endline (p.name ^ " loses: $" ^ (p.curr_bet |> string_of_float)) 
        in {p with bankroll = p.bankroll -. b} in

    match res2 with
    | W -> 
      let () = print_endline (p.name ^ " wins: $" ^ (p'.curr_bet |> string_of_float)) in
      {p' with bankroll = p'.bankroll +.b; curr_bet = 0.0}
    | WB -> let () = print_endline (p'.name ^ " wins with blackjack!: $" ^ 
                                    (p'.curr_bet |> string_of_float)) 
      in {p with bankroll = p'.bankroll +. p'.curr_bet; curr_bet = 0.0}
    | P -> 
      let () = print_endline (p.name ^ ": Push.") in
      {p' with bankroll = p'.bankroll; curr_bet = 0.0}

    | L -> 
      let () = print_endline (p.name ^ " loses: $" ^ (p'.curr_bet |> string_of_float)) in
      {p' with bankroll = p'.bankroll -. b; curr_bet = 0.0}

  let to_string p =
    let n = p.name in
    let bankroll = string_of_float(p.bankroll) in
    "Name: " ^ n ^ "\nBankroll: " ^ bankroll
end