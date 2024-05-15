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

let test_hand _ = assert_equal [] Hand.empty

let suite =
  "Card Test Suite"
  >::: [
         "test_compare" >:: test_compare;
         "test_string_of_card" >:: test_string_of_card;
         "test_hand" >:: test_hand;
       ]

let _ = run_test_tt_main suite
