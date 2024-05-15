open OUnit2
open Poker

let test_compare _ =
  (* let card1 = { Card.suit = Clubs; Card.rank = Two } in let card2 = {
     Card.suit = Spades; Card.rank = Three } in let card3 = { Card.suit =
     Hearts; Card.rank = Two } in *)
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

  (* assert_equal (-1) (compare { Card.suit = Clubs; Card.rank = Two } {
     Card.suit = Clubs; Card.rank = Ace }); assert_equal (-1) (compare {
     Card.suit = Hearts; Card.rank = Two } { Card.suit = Spades; Card.rank = Two
     }); assert_equal (-1) (compare { Card.suit = Spades; Card.rank = Three } {
     Card.suit = Clubs; Card.rank = Two }); assert_equal (-1) (compare {
     Card.suit = Diamonds; Card.rank = Two } { Card.suit = Spades; Card.rank =
     Ten }); assert_equal (-1) (compare { Card.suit = Hearts; Card.rank = Seven
     } { Card.suit = Clubs; Card.rank = Jack }); *)
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
  let card = { Card.suit = Diamonds; Card.rank = Ace } in
  assert_equal "Ace of Diamonds" (Card.string_of_card card)

let suite =
  "Card Test Suite"
  >::: [
         "test_compare" >:: test_compare;
         "test_string_of_card" >:: test_string_of_card;
       ]

let _ = run_test_tt_main suite
