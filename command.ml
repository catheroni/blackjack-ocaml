
type game_option = 
  | AddPlayer of string*float
  | RemovePlayer of string
  | Bankroll of string
  | DealHand
  | ChangeDeckSize of int
  | PrintPlayers
  | PrintTableStats
  | Quit

type bet = float

type hand_option =
  | Hit
  | Stick
  | DoubleDown
  | Split

exception Malformed

exception MalformedName

exception MalformedBet

let check_if_string_is_int x = 
  try ignore(int_of_string x); true
  with Failure _ -> false


let check_if_string_is_float x = 
  try ignore(float_of_string x); true
  with Failure _ -> false

let malformed_option f =
  let () = Printf.printf("Your command was malformed, please retry:\n") in
  f (read_line())

let rec parse_game_option x = 
  let spl = String.split_on_char ' ' (String.trim(x)) in
  if String.trim x = "" then
    malformed_option parse_game_option
  else
  if List.hd spl = "AddPlayer" then
    if not(List.length spl = 3) then
      malformed_option parse_game_option
    else 
      let p = List.hd (List.tl spl) in
      let b = List.hd (List.tl(List.tl spl)) in
      if check_if_string_is_float b then
        AddPlayer (p, float_of_string b)
      else  malformed_option parse_game_option
  else if List.hd spl = "RemovePlayer" then
    if not(List.length spl = 2) then
      malformed_option parse_game_option
    else 
      let p = List.hd (List.tl spl) in
      RemovePlayer p
  else if List.hd spl = "ChangeDeckSize" then
    if not(List.length spl = 2) then
      malformed_option parse_game_option
    else 
      let p = List.hd (List.tl spl) in
      if check_if_string_is_int p then
        ChangeDeckSize (int_of_string p)
      else
        malformed_option parse_game_option
  else if List.hd spl = "Bankroll" then
    if not(List.length spl = 2) then
      malformed_option parse_game_option
    else 
      let p = List.hd (List.tl spl) in
      Bankroll p
  else if List.hd spl = "DealHand" then
    DealHand
  else if List.hd spl = "PrintTableStats" then
    PrintTableStats
  else if List.hd spl = "PrintPlayers" then
    PrintPlayers
  else if List.hd spl = "Quit" then
    Quit
  else
    malformed_option parse_game_option

let rec parse_hand_option x = 
  let spl = String.split_on_char ' ' (String.trim(x)) in
  if not(List.length spl = 1) then
    malformed_option parse_hand_option
  else match List.hd spl with
    | "Hit" -> Hit
    | "Stick" -> Stick
    | "DoubleDown" -> DoubleDown
    | "Split" -> Split
    | _ -> malformed_option parse_hand_option

let parse_bet x = match x with
  | None -> 0.0
  | Some h -> h