open OUnit2
open Poker

(* Test cases for Card *)
let test_compare _ =
  assert_equal 1
    (Card.compare
       { Card.suit = Clubs; Card.rank = Ten }
       { Card.suit = Clubs; Card.rank = Two });
  assert_equal 0
    (Card.compare
       { Card.suit = Clubs; Card.rank = Two }
       { Card.suit = Hearts; Card.rank = Two });
  assert_equal 1
    (Card.compare
       { Card.suit = Spades; Card.rank = Five }
       { Card.suit = Diamonds; Card.rank = Two });
  assert_equal 1
    (Card.compare
       { Card.suit = Hearts; Card.rank = Jack }
       { Card.suit = Clubs; Card.rank = Seven });
  assert_equal 1
    (Card.compare
       { Card.suit = Diamonds; Card.rank = Four }
       { Card.suit = Hearts; Card.rank = Three });
  assert_equal (-1)
    (Card.compare
       { Card.suit = Clubs; Card.rank = Two }
       { Card.suit = Clubs; Card.rank = Ace });
  assert_equal 1
    (Card.compare
       { Card.suit = Spades; Card.rank = Three }
       { Card.suit = Clubs; Card.rank = Two });
  assert_equal (-1)
    (Card.compare
       { Card.suit = Diamonds; Card.rank = Two }
       { Card.suit = Spades; Card.rank = Ten });
  assert_equal (-1)
    (Card.compare
       { Card.suit = Hearts; Card.rank = Seven }
       { Card.suit = Clubs; Card.rank = Jack });
  assert_equal 0
    (Card.compare
       { Card.suit = Clubs; Card.rank = Two }
       { Card.suit = Clubs; Card.rank = Two });
  assert_equal 0
    (Card.compare
       { Card.suit = Spades; Card.rank = Three }
       { Card.suit = Spades; Card.rank = Three });
  assert_equal 0
    (Card.compare
       { Card.suit = Diamonds; Card.rank = Ten }
       { Card.suit = Diamonds; Card.rank = Ten });
  assert_equal 0
    (Card.compare
       { Card.suit = Hearts; Card.rank = Queen }
       { Card.suit = Hearts; Card.rank = Queen });
  assert_equal 0
    (Card.compare
       { Card.suit = Clubs; Card.rank = Ace }
       { Card.suit = Clubs; Card.rank = Ace })

let test_string_of_card _ =
  assert_equal "Two of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Two });
  assert_equal "Three of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Three });
  assert_equal "Four of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Four });
  assert_equal "Five of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Five });
  assert_equal "Six of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Six });
  assert_equal "Seven of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Seven });
  assert_equal "Eight of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Eight });
  assert_equal "Nine of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Nine });
  assert_equal "Ten of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Ten });
  assert_equal "Jack of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Jack });
  assert_equal "Queen of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Queen });
  assert_equal "King of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = King });
  assert_equal "Ace of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Ace });

  assert_equal "Two of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Two });
  assert_equal "Three of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Three });
  assert_equal "Four of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Four });
  assert_equal "Five of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Five });
  assert_equal "Six of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Six });
  assert_equal "Seven of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Seven });
  assert_equal "Eight of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Eight });
  assert_equal "Nine of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Nine });
  assert_equal "Ten of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Ten });
  assert_equal "Jack of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Jack });
  assert_equal "Queen of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Queen });
  assert_equal "King of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = King });
  assert_equal "Ace of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Ace });

  assert_equal "Two of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Two });
  assert_equal "Three of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Three });
  assert_equal "Four of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Four });
  assert_equal "Five of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Five });
  assert_equal "Six of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Six });
  assert_equal "Seven of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Seven });
  assert_equal "Eight of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Eight });
  assert_equal "Nine of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Nine });
  assert_equal "Ten of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Ten });
  assert_equal "Jack of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Jack });
  assert_equal "Queen of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Queen });
  assert_equal "King of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = King });
  assert_equal "Ace of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Ace });

  assert_equal "Two of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Two });
  assert_equal "Three of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Three });
  assert_equal "Four of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Four });
  assert_equal "Five of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Five });
  assert_equal "Six of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Six });
  assert_equal "Seven of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Seven });
  assert_equal "Eight of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Eight });
  assert_equal "Nine of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Nine });
  assert_equal "Ten of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Ten });
  assert_equal "Jack of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Jack });
  assert_equal "Queen of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Queen });
  assert_equal "King of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = King });
  assert_equal "Ace of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Ace })

(* Test cases for Game *)

(* Test cases for Hand *)
let test_hand_add _ =
  let card1 = { Card.suit = Diamonds; Card.rank = Ace } in
  let card2 = { Card.suit = Clubs; Card.rank = Two } in
  let card3 = { Card.suit = Spades; Card.rank = Jack } in
  let hand1 = Hand.add card1 Hand.empty in
  let hand2 = Hand.add card2 hand1 in
  let hand3 = Hand.add card3 hand2 in
  assert_equal [] Hand.empty;
  assert_equal [ card1 ] hand1;
  assert_equal [ card1; card2 ] hand2;
  assert_equal [ card1; card2; card3 ] hand3

let test_string_of_hand _ =
  let card1 = { Card.suit = Diamonds; Card.rank = Ace } in
  let card2 = { Card.suit = Clubs; Card.rank = Two } in
  let card3 = { Card.suit = Hearts; Card.rank = Queen } in

  (* Test an empty hand *)
  let hand_empty = Hand.empty in
  assert_equal "" (Hand.string_of_hand hand_empty);

  (* Test a hand with a single card *)
  let hand_single_card = Hand.add card1 Hand.empty in
  assert_equal "Ace of Diamonds, " (Hand.string_of_hand hand_single_card);

  (* Test a hand with multiple cards *)
  let hand_multiple_cards = Hand.add card1 Hand.empty in
  let hand_multiple_cards = Hand.add card2 hand_multiple_cards in
  let hand_multiple_cards = Hand.add card3 hand_multiple_cards in
  assert_equal "Ace of Diamonds, Two of Clubs, Queen of Hearts"
    (Hand.string_of_hand hand_multiple_cards);

  (* Test a hand with duplicate cards *)
  let hand_duplicate_cards = Hand.add card1 Hand.empty in
  let hand_duplicate_cards = Hand.add card1 hand_duplicate_cards in
  assert_equal "Ace of Diamonds, Ace of Diamonds"
    (Hand.string_of_hand hand_duplicate_cards)

(* let test_filt_for_straight _ = let hand1 = [ { Card.rank = Ace; Card.suit =
   Diamonds }; { Card.rank = Two; Card.suit = Clubs }; { Card.rank = Three;
   Card.suit = Hearts }; { Card.rank = Four; Card.suit = Spades }; { Card.rank =
   Five; Card.suit = Diamonds }; { Card.rank = Six; Card.suit = Hearts }; {
   Card.rank = Six; Card.suit = Hearts }; ] in let hand2 = [ { Card.rank = Two;
   Card.suit = Clubs }; { Card.rank = Three; Card.suit = Hearts }; { Card.rank =
   Four; Card.suit = Spades }; { Card.rank = Five; Card.suit = Diamonds }; {
   Card.rank = Six; Card.suit = Hearts }; { Card.rank = Jack; Card.suit =
   Diamonds }; { Card.rank = Queen; Card.suit = Clubs }; ] in let hand3 = [ {
   Card.rank = Five; Card.suit = Clubs }; { Card.rank = Six; Card.suit = Hearts
   }; { Card.rank = Six; Card.suit = Hearts }; { Card.rank = Eight; Card.suit =
   Spades }; { Card.rank = Nine; Card.suit = Clubs }; { Card.rank = Jack;
   Card.suit = Diamonds }; { Card.rank = Queen; Card.suit = Clubs }; ] in let
   hand4 = [ { Card.rank = Five; Card.suit = Clubs }; { Card.rank = Six;
   Card.suit = Hearts }; { Card.rank = Seven; Card.suit = Diamonds }; {
   Card.rank = Eight; Card.suit = Spades }; { Card.rank = Nine; Card.suit =
   Clubs }; { Card.rank = Jack; Card.suit = Diamonds }; { Card.rank = Queen;
   Card.suit = Clubs }; ] in let hand5 = [ { Card.rank = Six; Card.suit = Hearts
   }; { Card.rank = King; Card.suit = Hearts }; { Card.rank = Ace; Card.suit =
   Spades }; { Card.rank = Two; Card.suit = Diamonds }; { Card.rank = Jack;
   Card.suit = Diamonds }; { Card.rank = Queen; Card.suit = Clubs }; { Card.rank
   = King; Card.suit = Hearts }; ] in let hand6 = [ { Card.rank = Jack;
   Card.suit = Diamonds }; { Card.rank = Queen; Card.suit = Clubs }; { Card.rank
   = King; Card.suit = Hearts }; { Card.rank = Ace; Card.suit = Spades }; ] in
   assert_equal (failwith "filt_for_straight should only be applied to 7 card
   hands") (Hand.filt_for_straight hand6); assert_equal (failwith
   "filt_for_straight") (Hand.filt_for_straight hand1); assert_equal (failwith
   "filt_for_straight") (Hand.filt_for_straight hand2); assert_equal (failwith
   "filt_for_straight") (Hand.filt_for_straight hand3); assert_equal (failwith
   "filt_for_straight") (Hand.filt_for_straight hand4); assert_equal (failwith
   "filt_for_straight") (Hand.filt_for_straight hand5) *)

let suite =
  "Card Test Suite"
  >::: [
         "test_compare" >:: test_compare;
         "test_string_of_card" >:: test_string_of_card;
         "test_hand_add" >:: test_hand_add;
         "test_string_of_hand" >:: test_string_of_hand;
         (* "test_filt_for_straight" >:: test_filt_for_straight; *)
       ]

let _ = run_test_tt_main suite
