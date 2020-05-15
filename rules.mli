open Deck

type res = W | WB | P | L

(** [hand_val h] is a list of the potential values of hand h. *)
val hand_val : Deck.card list -> int list

(** [bust h_val] is [true] iff [h_val] contains only values over 21. *)
val bust : int list -> bool

(** [d_fin h_val] is [true] iff [h_val] cannot be changed with another card. *)
val d_fin : int list -> bool

(** [is_blackjack] is [true] iff [h_val] represents a blackjack *)
val is_blackjack : Deck.card list -> bool


(** [get_result d p] is [W] if [p] wins, [P] if [p] ties, [L] if [p] loses. *)
val get_result : Deck.card list -> Deck.card list -> res
