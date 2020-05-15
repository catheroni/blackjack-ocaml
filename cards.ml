

type suit = C | D | H | S

exception InvSuit

type value = Two | Three | Four | Five | Six 
           | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

exception InvValue

type t = suit*value

let make_card suit value = 
  let suit' = match suit with
    | "Clubs" -> C
    | "Diamonds" -> D
    | "Hearts" -> H
    | "Spades" -> S
    | _ -> raise InvSuit  in
  let value' = match value with
    | "Two" -> Two
    | "Three" -> Three
    | "Four" -> Four
    | "Five" -> Five
    | "Six" -> Six
    | "Seven" -> Seven
    | "Eight" -> Eight
    | "Nine" -> Nine
    | "Ten" -> Ten
    | "Jack" -> Jack
    | "Queen" -> Queen
    | "King" -> King
    | "Ace" -> Ace
    | _ -> raise InvValue in
  (suit',value')

let card_val c = match snd c with
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 10
  | Queen -> 10
  | King -> 10
  | Ace -> 11

let card_n c = match snd c with
  | Two -> "Two"
  | Three -> "Three"
  | Four -> "Four"
  | Five -> "Five"
  | Six -> "Six"
  | Seven -> "Seven"
  | Eight -> "Eight"
  | Nine -> "Nine"
  | Ten -> "Ten"
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"
  | Ace -> "Ace"

let card_suit c = match fst c with
  | C -> "Clubs"
  | D -> "Diamonds"
  | H -> "Hearts"
  | S -> "Spades"

let rand_card s v = 
  let suit = match s with
    | 0 -> C
    | 1 -> D
    | 2 -> H
    | 3 -> S
    | _ -> raise InvSuit in

  let value = match v with
    | 0 -> Two
    | 1 -> Three
    | 2 -> Four
    | 3 -> Five
    | 4 -> Six
    | 5 -> Seven
    | 6 -> Eight
    | 7 -> Nine
    | 8 -> Ten
    | 9 -> Jack
    | 10 -> Queen
    | 11 -> King
    | 12 -> Ace 
    | _ -> raise InvValue in
  (suit,value)

let is_ace c =
  snd c = Ace