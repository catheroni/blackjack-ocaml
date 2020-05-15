open Player
open Deck

module Seats : sig

  (** [pl] is an object in a seat. *)
  type pl

  (** [t] is the type of seats. *)
  type t

  exception SeatsFull

  exception PlayerDNE


  (** [empty_seats] is seats with no players sitting. *)
  val empty_seats : t

  (** [map f s] is seats [s] where [f] has been applied each seat's player in order.  *)
  val map : (Player.t -> Player.t) -> t -> t

  (** [fold f s] is seats [s] where [f] has been applied each seat's player in order.  *)
  val fold : (Player.t -> t -> t) -> t -> t

  (** [num_pls s] is [v] where v is the number of players sitting in [s]. *)
  val num_pls : t -> int

  (** [get_pl p_name s] is [p] with [p_name] in [s].
        Raises: [PlayerDNE] if [p] is not sitting at a seat. *)
  val get_pl : string -> t -> Player.t

  (** [names s] is a list of the names of players in seats. 
      If seat is [Empty], name is [Empty]. *)
  val names : t -> string list

  (** [sit_player p s] is seats [s] with [p] sitting in next available seat.
      Raises: [SeatsFull] if there is no available seat. *)
  val sit_player : Player.t -> t -> t

  (** [rem_player p s] is seats [s] with [p] with [p_name]'s seat empty.
      Raises: [PlayerDNE] if [p] is not sitting at a seat. *)
  val rem_player : string -> t -> t

  (** [deal_card_to_pl c p s] is [s] where player [p] has dealt [c]. 
        Raises: [PlayerDNE] if [p] is not in [s]. *)
  val deal_cards_to_pl : Deck.card list -> Player.t -> t -> t

  (** [deal_card_to_pl_s c p s] is [s] where player [p] has dealt [c]. 
        Raises: [PlayerDNE] if [p] is not in [s]. *)
  val deal_cards_to_pl_s : Deck.card list -> Player.t -> t -> t  

  (** [player_list s] is [p1 = seat1; ... ; pn = seatn], where 
      [p1] is [Some p1] if seat is occupied and [None] if seat is [Empty].*)
  val to_list : t -> Player.t option list

  (** [update_player p p' s] is [s] where [p]'s seat contains [p'].
      Raises: [PlayerDNE] if [p] is not sitting at a seat. *)
  val update_player: Player.t -> Player.t -> t -> t
end