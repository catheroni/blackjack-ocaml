open Deck
open Rules

(** A [Player] is a module representing a player and the functions of them. *)
module Player : sig

  (** [card] is the type of element in a players hand *)
  type card = Deck.card

  (** [t] is the type of players. *)
  type t

  exception InsuffFunds

  (** [init_player n b] is a player with name = [n], bankroll = [b]
      and an empty hand. *)
  val init_player : string -> float -> t

  (** [player_name p] is string [name] of player [p]. *)
  val player_name : t -> string

  (** [curr_hand p] is a set-like list matching the current hand of [p]. *)
  val curr_hand : t -> card list

  (** [spl_hand p] is a set-like list matching the split hand of [p]. *)
  val spl_hand : t -> card list option

  (** [cards_match p] is [true] if [p]'s hand contains 2 cards of the matching value
        and [false] if otherwise.  *)
  val cards_match: t -> bool  

  (** [curr_bankroll p] is the current bankroll of [p]. *)
  val curr_bankroll : t -> float

  (** [curr_bet p] is the current bet of [p]. *)
  val curr_bet : t -> float

  (** [change_n n p] is [p] where [p.name] is [n]. *)
  val change_n : string -> t -> t

  (** [initial_split p] *)
  val initial_split : t -> t

  (** [deal_card c p] is [p] with [c] inserted into their current hand. *)
  val deal_card_to_spl_hand : card -> t -> t

  (** [deal_card c p] is [p] with [c] inserted into their current hand. *)
  val deal_card_to_hand : card -> t -> t

  (** [clear_hand p] is [p] with an empty hand *)
  val clear_hand : t -> t

  (** [change_bankroll p amt] is [p] where 
      [p]'s bankroll is [P.curr_bankroll p] + [cng_amt]. *)
  val change_bankroll : float -> t -> t

  (** [make_bet amt p] is [p] where curr_bet is increased by 
      [amt] and bankroll is decreased by [amt].
      Raises: [InsuffFunds] if [amt] > [p.bankroll] *)
  val make_bet : float -> t -> t

  (** [eval_bet res p] is [p] where [p.curr_bet] = [0.0].
        [p.bankroll] is [p.bankroll += 2*curr_bet] if [res] = [W]
        [p.bankroll] is [p.bankroll += curr_bet] if [res] = [P]
        [p.bankroll] is [p.bankroll] if [res] = [L]. *)
  val eval_bet : t -> Rules.res -> t

  (** [eval_bet_with_split res p] is like eval_bet except it evaluates the
      split hand of a player*)
  val eval_bet_with_split : t -> Rules.res -> Rules.res -> t

  (**[to_string p] is a string representation of player *)
  val to_string : t -> string
end