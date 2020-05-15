open Player
open Deck

module Seats = struct

  type pl = Pl of Player.t | Empty

  type t = pl list

  exception SeatsFull

  exception PlayerDNE

  let empty_seats = [Empty; Empty; Empty; Empty; Empty; Empty; Empty]

  let f_pl f x = match x with
    | Empty -> Empty
    | Pl v -> Pl (f v)

  let rec map (f:(Player.t -> Player.t)) s = 
    match s with
    | [] -> []
    | h::t -> [f_pl f h] @ map f t

  let fold f s = failwith "Unimplemented"

  let rec num_pls s = match s with
    | [] -> 0
    | h::t -> match h with
      | Empty -> num_pls t
      | Pl v -> 1 + num_pls t

  let rec get_pl p_name s = match s with
    | [] -> raise PlayerDNE
    | h::t -> match h with
      | Empty -> get_pl p_name t
      | Pl v -> if Player.player_name v = p_name then v else get_pl p_name t

  let update_player p p' s = 
    let rec f pls = match pls with
      | [] -> []
      | h::t -> if h = Pl p then [Pl p'] @ t else [h] @ f t in
    if List.mem (Pl p) s then f s else raise PlayerDNE

  let rec names s = match s with
    | [] -> []
    | h::t -> match h with
      | Empty -> ["Empty"] @ names t
      | Pl v -> [Player.player_name v] @ names t

  let p_to_pl p = Pl p

  let sit_player p s = 
    let rec f pls = match pls with
      | [] -> []
      | h::t -> if h = Empty then [p_to_pl p] @ t else [h] @ f t in
    if List.mem Empty s then f s else raise SeatsFull

  let rem_player p_name (s:pl list) = 
    let rec f pls = match pls with
      | [] -> []
      | h::t -> match h with
        | Empty -> [Empty] @ f t
        | Pl v -> if Player.player_name v = p_name then [Empty] @ f t else [Pl v] @ f t in
    if names s |> List.mem p_name then f s else raise PlayerDNE

  let deal_cards_to_pl c p s = 
    let rec deal_cards cards p' = match cards with
      | [] -> p'
      | h::t -> deal_cards t (Player.deal_card_to_hand h p') in
    let f pl = if pl = p then deal_cards c pl else pl in
    map f s

  let deal_cards_to_pl_s c p s =
    let rec deal_cards cards p' = match cards with
      | [] -> p'
      | h::t -> deal_cards t (Player.deal_card_to_spl_hand h p') in
    let f pl = if pl = p then deal_cards c pl else pl in
    map f s

  let rec to_list s = match s with
    | [] -> []
    | h::t -> match h with
      | Empty -> [None] @ to_list t
      | Pl v -> [Some v] @ to_list t

end