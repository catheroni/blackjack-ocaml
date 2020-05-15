open Deck
open Player
open Seats



(** [t] is the type of a table. *)
type t 

(** [empty_table] represents the empty table. *)
val empty_table : t

(** [rep_ok t] returns [d] if [d] satisfies its representation
    invariants. 
    Raises: [Failure] with an unspecified error message
      if [t] does not satisfy its representation invariants. *)
val rep_ok : t -> t

(** [player_names t] is a list of the names of the players currently at 
      table [t]. *)
val player_names : t -> string list

(** [get_player pls n] is [p] with name [n] in [pls]. *)
val get_player : Player.t list -> string -> Player.t

(** [add_player t n b] is [t] with added player [p].
    [P]'s name is [n] and [p]'s bankroll is [b]. *)
val add_player : t -> string -> float -> t

(** [remove_player t n] is table [t] with
    player that has name [n] removed. *)
val remove_player : t -> string -> t

(** [change_player_name t curr_n new_n] is [t] with player
    with name [new_n] *)
val change_player_name : t -> string -> string -> t

(** [deal_card_to_player t n] is table [t] containing
    player with name [n] after being dealt 
    card with value [v] from deck [t.deck]. *)
val deal_card_to_player : t -> string -> t

(** [deal_card_to_dealer t] is [t] with 
    dealer having been dealt a card from [t.deck]. *)
val deal_card_to_dealer : t -> t

(** [clear_hands t] is table [t] where 
    all player hands and dealer hands are empty. *)
val clear_hands : t -> t

(** [player_hand t n] is a set-like list of 
    player with name [n]'s hand. *)
val player_hand : t -> string -> Deck.card list

(** [dealer_hand t] is a set-like list of the dealer's hand at table [t]. *)
val dealer_hand : t -> Deck.card list

(** [player_hand_value t n] is the value [v] of
    player with name [n]'s current hand. *)
val player_hand_value : t -> string -> int list

(** [player_bankroll t n] is player at table [t] with name [n]'s bankroll. *)
val player_bankroll : t -> string -> float

(** [change_player_bankroll t n cng_amt] is [t] with player [p].
    [p]'s bankroll is [P.curr_bankroll p] + [cng_amt]. *)
val change_player_bankroll : t -> string -> float -> t

(** [eval_hand t] is [t] with updated bankrolls for players based off of blackjack rules. 
    Requires: hand is fully dealt. *)
val eval_hand : t -> t

(** [player_bet p_name bet t] is table [t] with player with name [p_name
    having made the bet] *)
val player_bet : string -> float -> t -> t

(** [iter_players f t] is [t] where [f] is applied to each player [p] in [t]. *)
val iter_players : (string -> t -> t) -> t -> t

(** [run_hand_for_player p t] is [t] where player [p] has run through their hand. *)
val run_hand_for_player : string -> t -> t

(** [run_hand_for_players t] is [t] where each player has run through their hand. *)
val run_hand_for_players : t -> t

(** [run_bet_for_player p_name t] *)
val run_bet_for_player : string -> t -> t

(** [run_bet_for_players t] *)
val run_bet_for_players : t -> t

(** [deal_new_hand t] is [t] with new hand dealt to each player and dealer *)
val deal_new_hand : t -> t

(**[to_string t] is a string representation of [t] *)
val to_string : t -> string

(**[get_table_stats t] tells you how many players and decks there are *)
val get_table_stats : t -> string

(**[change_deck_size t n] returns an [t] with [n] decks *)
val change_deck_size : t -> int -> t
