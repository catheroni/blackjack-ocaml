open Card

module Deck = struct

  type card = Card.t

  type t = {
    name : string;
    cards_removed : card list;
    number_of_decks : int;
  }

  let deck_size = 52
  let shuffle_threshold = 0.75

  let rep_ok d = 
    if List.length d.cards_removed > int_of_float(float_of_int(deck_size) *. 
                                                  shuffle_threshold *. float_of_int(d.number_of_decks))
    then failwith "Deck Unshuffled" else d

  let init_deck deck_name number_decks =
    {name = deck_name; cards_removed = []; number_of_decks = number_decks}

  let get_num_decks d =
    d.number_of_decks
  let is_full d = 
    d.cards_removed = []

  let size d = 
    (deck_size*d.number_of_decks) - List.length d.cards_removed

  let all_removed c removed num_decks =
    let num_removed = List.fold_left (fun a x -> if x = c then a + 1 else a) 0 
        removed in if num_removed == num_decks then true else false

  let remove c d = 
    if all_removed c d.cards_removed d.number_of_decks then d else
      let c_removed' = [c] @ d.cards_removed in
      {name = d.name; cards_removed = c_removed'; number_of_decks = d.number_of_decks}

  let insert c d = 
    let f x = not (x = c) in
    if all_removed c d.cards_removed d.number_of_decks then
      let c_removed' = List.filter f d.cards_removed in 
      {name = d.name; cards_removed = c_removed'; number_of_decks = d.number_of_decks}
    else d

  let member c d = 
    all_removed c d.cards_removed d.number_of_decks |> not

  let shuffle d =
    {d with cards_removed = []}

  let rec deal_card d = 
    if List.length d.cards_removed = int_of_float(float_of_int(deck_size)
                                                  *.shuffle_threshold *. float_of_int(d.number_of_decks))
    then shuffle d |> deal_card else
      let rec rand_card removed =
        let () = Random.self_init() in
        let s = Random.int 4 in
        let v = Random.int 13 in
        let c = Card.rand_card s v in
        if all_removed c removed d.number_of_decks then rand_card removed else c in
      let dealt_card = rand_card d.cards_removed in
      let upd_deck = remove dealt_card d in
      (dealt_card,upd_deck)

  let deal_cards n d = 
    let rec f num_cards deck cards= if num_cards = 0 then (cards,deck) 
      else let dealt = deal_card deck in
        [fst dealt] @ cards |>  f (num_cards - 1) (snd dealt) in
    f n d []

  let to_list d = 
    let suits = ["Clubs";"Diamonds";"Hearts";"Spades"] in
    let values = ["Two";"Three";"Four";"Five";"Six";"Seven";"Eight";"Nine";"Ten";
                  "Jack";"Queen";"King";"Ace"] in 
    let build_deck suits values : (string*string) list = 
      List.fold_left (fun acc1 ele1 ->
          List.fold_left (fun acc2 ele2 -> (ele1,ele2)::acc2) acc1 values) 
        [] suits in let full_deck = List.map (fun x -> make_card (fst x) (snd x)) 
                        (build_deck suits values) in
    List.filter (fun x -> List.mem x d.cards_removed) full_deck

  let format fmt d = failwith "Unimplemented"

end