open Command
open Player
open Deck

type gamestate = TableIdle | HandRunning

type t 

val print_options : string list -> unit

val print_cards : string -> Deck.card list -> unit

val run_idle : t -> t

val run_hand_for_dealer : Table.t -> Table.t

val run_hand : t -> t

(** [game_thread t g] *)
val game_thread : t -> t