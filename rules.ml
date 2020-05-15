open Deck

type res = W | WB | P | L

type hand_res = V of int | Bust

let rec num_aces h = match h with
  | [] -> 0
  | h::t -> if Card.is_ace h then 1 + num_aces t else num_aces t

let hand_val h = 
  let rec f hand = match hand with
    | [] -> 0
    | h::t -> Card.card_val h + f t in
  let rec f' aces max_val = if aces = 0 then [max_val] 
    else [max_val - (aces*10)] @ f' (aces-1) max_val in
  f h |> f' (num_aces h)

let rec bust v = match v with
  | [] -> true
  | h::t -> if h <= 21 then false else bust t

let rec is_blackjack v = 
  if not(List.length v = 2) then false
  else let hval = hand_val v in
  if List.hd hval = 21 || (List.length hval = 2 && List.hd(List.tl hval) = 21) 
  then true else false

let rec d_fin v = match v with
  | [] -> false
  | h::t -> if h >= 17 then true else d_fin t

let get_hand_res h =
  let potential_vals = hand_val h in
  let max = potential_vals |> List.filter (fun x -> x <= 21) 
            |> List.fold_left (fun v1 v2 -> if v1 >= v2 then v1 else v2) 0 in
  if bust potential_vals then Bust else V max

let compare_hand_vals dealer_val player_val = match dealer_val - player_val with
  | w when w < 0 -> W
  | p when p = 0 -> P
  | _ -> L

let get_result dealer_h player_h =
  let d_val = get_hand_res dealer_h in
  let p_val = get_hand_res player_h in
  match p_val with
  | Bust -> L
  | V p_v -> match d_val with
    | Bust -> W
    | V d_v -> if is_blackjack player_h then WB else 
      compare_hand_vals d_v p_v