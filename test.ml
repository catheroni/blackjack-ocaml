(** TEST PLAN 
    In this test suite, we are using OUnit to automatically test much of the 
    functionality of how decks of cards are being built, whether player information
    is being updated properly, and whether table information is being updated 
    properly. Since our game involves some level of randomization (e.g. random cards
    being dealt), the testing for parts of the system which relied on randomization 
    were tested manually. We used OUnit to test the Deck, Player, and Table modules
    while the rest of the modules were manually tested through the terminal. The 
    test cases for the modules in this suite were developed using black box testing 
    for each function. *)
open OUnit2
open Card
open Deck
open Player
open Table

let test_vals name v1 v2 : test =
  name >:: (fun _ -> assert_equal v1 v2)

let deck_tests = 
  let full = Deck.init_deck "full deck" 1 in
  let ace_of_spades = Card.make_card "Spades" "Ace" in
  let test_deck1 = Deck.remove ace_of_spades full in
  let test_deck2 = Deck.deal_cards 2 full |> snd in
  let test_deck3 = Deck.deal_cards 0 test_deck2 |> snd in
  let two_decks = Deck.init_deck "full deck" 2 in
  let two_decks_one_ace = Deck.remove ace_of_spades two_decks in
  let two_decks_no_ace = Deck.remove ace_of_spades two_decks_one_ace in

  [
    Deck.is_full full |> test_vals "[init_deck] with [is_full]" true;
    Deck.size full |> test_vals "[init_deck] with [size]" 52;
    Deck.size test_deck1 |> test_vals "[test_deck1] with [size]" 51;
    Deck.member ace_of_spades test_deck1 |> test_vals "[member] with 
      [test_deck1]" false;
    Deck.deal_card full |> snd |> Deck.size |> test_vals "[deal_card] with 
      [size] on [full]" 51;
    Deck.deal_cards 1 full |> snd |> Deck.size |> test_vals "[deal_cards] with
     [0]" 51;
    Deck.size test_deck2 |> test_vals "[test_deck2] with [size]" 50;
    Deck.is_full test_deck3 |> test_vals "[size] with [test_deck3]" false;
    Deck.is_full two_decks |> test_vals "[is_full] on two decks" true;
    Deck.is_full two_decks_no_ace |> test_vals "[is_full] on two decks with 
      removal" false;
    Deck.size two_decks |> test_vals "[size] on two decks" 104;
    Deck.deal_card two_decks |> snd |> Deck.size |> test_vals "[deal_card] on 
      two decks" 103;
    Deck.member ace_of_spades two_decks_one_ace |> test_vals "[member] on 
      two decks" true;
    Deck.deal_cards 2 two_decks |> snd |> Deck.size |> test_vals "[deal_cards]
      two decks" 102;
    Deck.deal_cards 0 two_decks |> snd |> Deck.size |> test_vals "[deal_cards]
      nothing removed" 104;
    Deck.deal_cards 3 full |> snd |> Deck.size |> test_vals "deal multiple from
      one deck" 49;
    Deck.deal_cards 0 two_decks |> snd |> Deck.size |> test_vals "deal 0 from 
      one deck" 104;
  ]

let player_tests = 
  let player = Player.init_player "Kyle" 100.0 in
  let ace_of_spades = Card.make_card "Spades" "Ace" in
  let player_with_card = Player.deal_card_to_hand ace_of_spades player in
  let player2 = Player.make_bet 10.0 player in
  [
    Player.player_name player |> test_vals "player name" "Kyle";
    Player.curr_hand player |> test_vals "current hand empty" [];
    Player.curr_bankroll player |> test_vals "current bankroll" 100.0;
    Player.curr_bet player |> test_vals "current bet init" 0.0;
    Player.change_n "Gary" player |> Player.player_name |> test_vals "change
      Kyle to Gary" "Gary";
    Player.deal_card_to_hand ace_of_spades player |> Player.curr_hand |> 
    test_vals "deal_card to player" [ace_of_spades]; 
    Player.deal_card_to_hand ace_of_spades player |> Player.clear_hand|> 
    Player.curr_hand |> test_vals "clear_hand" [];
    Player.change_bankroll 100.0 player |> Player.curr_bankroll |> test_vals 
      "change bankroll" 200.0;
    Player.make_bet 10.0 player |> Player.curr_bet |> test_vals "bet changes 
      after make_bet" 10.0;
    Player.eval_bet player2 W |> Player.curr_bankroll |> test_vals 
      "eval_bet with W" 110.0;
    Player.eval_bet player2 W |> Player.curr_bet |> test_vals "bet after
     eval_bet" 0.0;
    Player.eval_bet player2 P |> Player.curr_bankroll |> test_vals 
      "eval_bet with P" 100.0;
    Player.eval_bet player2 L |> Player.curr_bankroll |> test_vals 
      "eval_bet with L" 90.0;
    Player.make_bet 10.0 player2 |> Player.curr_bet |> test_vals "player2 
      bet updates from [make_bet]" 20.0;
    Player.deal_card_to_hand ace_of_spades player_with_card |> Player.cards_match |> 
    test_vals "cards match true" true; 
    Player.cards_match player |> test_vals "cards match false (0 cards)" false;
    Player.cards_match player_with_card |> test_vals "cards match false (1 
      card)" false;  
  ]

let table_tests = 
  let empty = Table.empty_table in
  let table1 = Table.add_player empty "Kyle" 100.0 in
  let table2 = Table.add_player table1 "Gary" 50.0 in
  let table3 = Table.add_player table2 "Catherine" 50.0 in
  let table4 = Table.remove_player table3 "Catherine" in
  let table5 = Table.change_player_bankroll table3 "Kyle" 50.0 in
  let table6 = Table.deal_card_to_player table3 "Kyle" in
  let table7 = Table.clear_hands table6 in

  [
    Table.player_names empty |> test_vals "table player names"
      ["Empty";"Empty";"Empty";"Empty";"Empty";"Empty";"Empty"];
    Table.player_names table3 |> test_vals "table3 player names" 
      ["Kyle";"Gary";"Catherine";"Empty";"Empty";"Empty";"Empty"];
    Table.player_names table4 |> test_vals "removed player" 
      ["Kyle";"Gary";"Empty";"Empty";"Empty";"Empty";"Empty"];
    Table.player_hand table3 "Kyle" |> test_vals "Kyle init player hand" [];
    Table.player_hand table3 "Catherine" |> test_vals "Gary init player hand" 
      [];
    Table.player_hand table3 "Catherine" |> test_vals "Catherine init player 
      hand" [];
    Table.change_player_name table3 "Kyle" "Nate" |> Table.player_names |> 
    test_vals "Kyle name changed" 
      ["Nate";"Gary";"Catherine";"Empty";"Empty";"Empty";"Empty"];
    Table.change_player_name table3 "Gary" "Nate" |> Table.player_names 
    |> test_vals "Gary name changed" 
      ["Kyle";"Nate";"Catherine";"Empty";"Empty";"Empty";"Empty"];
    Table.change_player_name table3 "Catherine" "Nate" |> Table.player_names 
    |> test_vals "Catherine name changed" 
      ["Kyle";"Gary";"Nate";"Empty";"Empty";"Empty";"Empty"];
    Table.player_bankroll table3 "Kyle" |> test_vals "Kyle bankroll" 100.0;
    Table.player_bankroll table3 "Gary" |> test_vals "Gary bankroll" 50.0;
    Table.player_bankroll table3 "Catherine" |> test_vals "Catherine 
      bankroll" 50.0;
    Table.player_bankroll table5 "Kyle" |> test_vals "Kyle bankroll 
      changed" 150.0;
    Table.player_hand table7 "Kyle" |> test_vals "clear hands" [];
    Table.player_hand_value table7 "Kyle" |> test_vals "player value Kyle" [0];
    Table.player_hand_value table3 "Gary" |> test_vals "player value Gary" [0];
    Table.player_hand_value table7 "Catherine" |> test_vals "hand value C" [0];

  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    deck_tests;
    player_tests;
    table_tests;
  ]

let _ = run_test_tt_main suite
