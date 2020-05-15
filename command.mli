

(** A [game_option] is a variant type representing the input options a 
    player can make while no hand is running. *)
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

(** A [hand_option] is a variant type representing the input options a
    player can make while a hand in running. *)
type hand_option =
  | Hit
  | Stick
  | DoubleDown
  | Split

(** Raised when an option is malformed. *)
exception Malformed

exception MalformedName

exception MalformedBet

(** [parse_game_option x] takes in a string and converts it to a [game_option] *)
val parse_game_option : string -> game_option

(** [parse_hand_option x] takes in a string and converts it to a [hand_option] *)
val parse_hand_option : string -> hand_option

(** [parse_bet x] takes in a and converts it to a [hand_option] *)
val parse_bet : float option -> bet