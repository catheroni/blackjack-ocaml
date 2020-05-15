(**
   Representation of a card.
*)

(** The abstract type of values representing cards *)
type t

val make_card : string -> string -> t

val card_val : t -> int

val card_n : t -> string

val card_suit : t -> string

val rand_card : int -> int -> t

val is_ace : t -> bool