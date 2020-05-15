open Card

(** A [Deck] represents the current status of a deck of cards*)
module Deck : sig

  (** [card] is the type of card in the deck *)
  type card = Card.t

  (** [t] is the type of decks. 
      The deck is shuffled when there is 25% of the deck remaining. *)
  type t

  (** [rep_ok d] returns [d] if [d] satisfies its representation
      invariants.
      Raises: [Failure] with an unspecified error message
        if [d] does not satisfy its representation invariants. *)
  val rep_ok : t  -> t

  (** [init_deck n x] is a full deck with name n and number of decks x *)
  val init_deck : string -> int -> t

  (** [get_num_decks t] returns how many decks there are in this deck *)
  val get_num_decks : t -> int
  (** [is_full d] is [true] iff [d] is full. *)
  val is_full : t -> bool

  (** [size d] is the number of cards in [d]. *
      [size empty] is [0]. *)
  val size : t -> int

  (** [remove k d] contains all the bindings of [d] except
      a binding for [k].  If [k] is not bound in [d], then
      [remove] returns a dictionary with the same bindings
      as [d]. *)
  val remove : card -> t -> t

  (** [insert c d] is [d] with [c]. If [c] was already
      a member, [insert c d] is [d]. *)
  val insert : card -> t -> t

  (** [member k d] is [true] iff [k] is bound in [d]. *)
  val member : card -> t -> bool

  (** [deal d] is [(c,d')] where [c] is the dealt card, and [d'] is the new deck. *)
  val deal_card : t -> card*t

  (** [deal_cards n d] is ([c0;...;cn],d). 
        Requires: [n] >= [0]. *)
  val deal_cards : int -> t -> (card list)*t

  (** [to_list d] is a set-like list containing the same
      cards as [d]. *)
  val to_list : t -> card list

  (** [format] is a printing function suitable for use
      with the toplevel's [#install_printer] directive.
      It outputs a textual representation of a dictionary
      on the given formatter. *)
  val format : Format.formatter -> t -> unit
end