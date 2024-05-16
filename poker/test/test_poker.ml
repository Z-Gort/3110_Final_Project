open OUnit2
open Poker

(* Test cases for Card *)
let test_compare1 _ =
  assert_equal 1
    (Card.compare
       { Card.suit = Clubs; Card.rank = Ten }
       { Card.suit = Clubs; Card.rank = Two })

let test_compare2 _ =
  assert_equal 0
    (Card.compare
       { Card.suit = Clubs; Card.rank = Two }
       { Card.suit = Hearts; Card.rank = Two })

let test_compare3 _ =
  assert_equal 1
    (Card.compare
       { Card.suit = Spades; Card.rank = Five }
       { Card.suit = Diamonds; Card.rank = Two })

let test_compare4 _ =
  assert_equal 1
    (Card.compare
       { Card.suit = Hearts; Card.rank = Jack }
       { Card.suit = Clubs; Card.rank = Seven })

let test_compare5 _ =
  assert_equal 1
    (Card.compare
       { Card.suit = Diamonds; Card.rank = Four }
       { Card.suit = Hearts; Card.rank = Three })

let test_compare6 _ =
  assert_equal (-1)
    (Card.compare
       { Card.suit = Clubs; Card.rank = Two }
       { Card.suit = Clubs; Card.rank = Ace })

let test_compare7 _ =
  assert_equal 1
    (Card.compare
       { Card.suit = Spades; Card.rank = Three }
       { Card.suit = Clubs; Card.rank = Two })

let test_compare8 _ =
  assert_equal (-1)
    (Card.compare
       { Card.suit = Diamonds; Card.rank = Two }
       { Card.suit = Spades; Card.rank = Ten })

let test_compare9 _ =
  assert_equal (-1)
    (Card.compare
       { Card.suit = Hearts; Card.rank = Seven }
       { Card.suit = Clubs; Card.rank = Jack })

let test_compare10 _ =
  assert_equal 0
    (Card.compare
       { Card.suit = Clubs; Card.rank = Two }
       { Card.suit = Clubs; Card.rank = Two })

let test_compare11 _ =
  assert_equal 0
    (Card.compare
       { Card.suit = Spades; Card.rank = Three }
       { Card.suit = Spades; Card.rank = Three })

let test_compare12 _ =
  assert_equal 0
    (Card.compare
       { Card.suit = Diamonds; Card.rank = Ten }
       { Card.suit = Diamonds; Card.rank = Ten })

let test_compare13 _ =
  assert_equal 0
    (Card.compare
       { Card.suit = Hearts; Card.rank = Queen }
       { Card.suit = Hearts; Card.rank = Queen })

let test_compare14 _ =
  assert_equal 0
    (Card.compare
       { Card.suit = Clubs; Card.rank = Ace }
       { Card.suit = Clubs; Card.rank = Ace })

let test_string_of_card1 _ =
  assert_equal "Two of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Two })

let test_string_of_card2 _ =
  assert_equal "Three of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Three })

let test_string_of_card3 _ =
  assert_equal "Four of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Four })

let test_string_of_card4 _ =
  assert_equal "Five of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Five })

let test_string_of_card5 _ =
  assert_equal "Six of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Six })

let test_string_of_card6 _ =
  assert_equal "Seven of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Seven })

let test_string_of_card7 _ =
  assert_equal "Eight of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Eight })

let test_string_of_card8 _ =
  assert_equal "Nine of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Nine })

let test_string_of_card9 _ =
  assert_equal "Ten of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Ten })

let test_string_of_card10 _ =
  assert_equal "Jack of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Jack })

let test_string_of_card11 _ =
  assert_equal "Queen of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Queen })

let test_string_of_card12 _ =
  assert_equal "King of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = King })

let test_string_of_card13 _ =
  assert_equal "Ace of Clubs"
    (Card.string_of_card { Card.suit = Clubs; Card.rank = Ace })

let test_string_of_card14 _ =
  assert_equal "Two of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Two })

let test_string_of_card15 _ =
  assert_equal "Three of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Three })

let test_string_of_card16 _ =
  assert_equal "Four of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Four })

let test_string_of_card17 _ =
  assert_equal "Five of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Five })

let test_string_of_card18 _ =
  assert_equal "Six of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Six })

let test_string_of_card19 _ =
  assert_equal "Seven of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Seven })

let test_string_of_card20 _ =
  assert_equal "Eight of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Eight })

let test_string_of_card21 _ =
  assert_equal "Nine of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Nine })

let test_string_of_card22 _ =
  assert_equal "Ten of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Ten })

let test_string_of_card23 _ =
  assert_equal "Jack of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Jack })

let test_string_of_card24 _ =
  assert_equal "Queen of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Queen })

let test_string_of_card25 _ =
  assert_equal "King of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = King })

let test_string_of_card26 _ =
  assert_equal "Ace of Spades"
    (Card.string_of_card { Card.suit = Spades; Card.rank = Ace })

let test_string_of_card27 _ =
  assert_equal "Two of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Two })

let test_string_of_card28 _ =
  assert_equal "Three of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Three })

let test_string_of_card29 _ =
  assert_equal "Four of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Four })

let test_string_of_card30 _ =
  assert_equal "Five of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Five })

let test_string_of_card31 _ =
  assert_equal "Six of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Six })

let test_string_of_card32 _ =
  assert_equal "Seven of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Seven })

let test_string_of_card33 _ =
  assert_equal "Eight of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Eight })

let test_string_of_card34 _ =
  assert_equal "Nine of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Nine })

let test_string_of_card35 _ =
  assert_equal "Ten of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Ten })

let test_string_of_card36 _ =
  assert_equal "Jack of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Jack })

let test_string_of_card37 _ =
  assert_equal "Queen of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Queen })

let test_string_of_card38 _ =
  assert_equal "King of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = King })

let test_string_of_card39 _ =
  assert_equal "Ace of Hearts"
    (Card.string_of_card { Card.suit = Hearts; Card.rank = Ace })

let test_string_of_card40 _ =
  assert_equal "Two of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Two })

let test_string_of_card41 _ =
  assert_equal "Three of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Three })

let test_string_of_card42 _ =
  assert_equal "Four of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Four })

let test_string_of_card43 _ =
  assert_equal "Five of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Five })

let test_string_of_card44 _ =
  assert_equal "Six of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Six })

let test_string_of_card45 _ =
  assert_equal "Seven of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Seven })

let test_string_of_card46 _ =
  assert_equal "Eight of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Eight })

let test_string_of_card47 _ =
  assert_equal "Nine of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Nine })

let test_string_of_card48 _ =
  assert_equal "Ten of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Ten })

let test_string_of_card49 _ =
  assert_equal "Jack of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Jack })

let test_string_of_card50 _ =
  assert_equal "Queen of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = Queen })

let test_string_of_card51 _ =
  assert_equal "King of Diamonds"
    (Card.string_of_card { Card.suit = Diamonds; Card.rank = King })

let test_string_of_card52 _ =
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

let test_check_straight_flush _ =
  let seven_cards =
    Hand.add
      { Card.suit = Diamonds; Card.rank = Eight }
      (Hand.add
         { Card.suit = Diamonds; Card.rank = Seven }
         (Hand.add
            { Card.suit = Diamonds; Card.rank = Six }
            (Hand.add
               { Card.suit = Diamonds; Card.rank = Five }
               (Hand.add
                  { Card.suit = Diamonds; Card.rank = Four }
                  (Hand.add
                     { Card.suit = Diamonds; Card.rank = Three }
                     (Hand.add
                        { Card.suit = Diamonds; Card.rank = Two }
                        Hand.empty))))))
  in
  assert_equal
    (Some
       [
         { Card.suit = Diamonds; Card.rank = Four };
         { Card.suit = Diamonds; Card.rank = Five };
         { Card.suit = Diamonds; Card.rank = Six };
         { Card.suit = Diamonds; Card.rank = Seven };
         { Card.suit = Diamonds; Card.rank = Eight };
       ])
    (Hand.check_straight_flush seven_cards)

(* Test cases for Player *)
let test_new_user _ =
  assert_equal Player.User Player.new_user.player_type;
  assert_equal Hand.empty Player.new_user.hand;
  assert_equal Player.default_chips Player.new_user.chips;
  assert_equal false Player.new_user.folded

let test_new_bot _ =
  let b = Player.new_bot 1 in
  assert_equal (Player.Bot 1) b.player_type;
  assert_equal Hand.empty b.hand;
  assert_equal Player.default_chips b.chips;
  assert_equal false b.folded

let test_none_player _ =
  assert_equal Player.None Player.none_player.player_type;
  assert_equal Hand.empty Player.none_player.hand;
  assert_equal Int.min_int Player.none_player.chips;
  assert_equal false Player.none_player.folded

let test_subtract_chips _ =
  let p = Player.new_user in
  assert_equal (p.chips - 10) (Player.subtract_chips p 10).chips

let test_add_chips _ =
  let p = Player.new_user in
  assert_equal (p.chips + 10) (Player.add_chips p 10).chips

let test_deal_card _ =
  let p = Player.new_user in
  let card = { Card.suit = Spades; Card.rank = Two } in
  assert_equal 1 (List.length (Player.deal_card p card).hand)

let test_fold _ =
  let p = Player.new_user in
  assert_equal true (Player.fold p).folded

let test_print_player _ =
  let p = Player.new_user in
  assert_equal
    (print_endline "Player type: User";
     print_endline "Hand: ";
     print_endline "Chips: 100";
     print_endline "Folded: false")
    (Player.print_player p);
  let b = Player.new_bot 1 in
  assert_equal
    (print_endline "Player type: Bot 1";
     print_endline "Hand: ";
     print_endline "Chips: 100";
     print_endline "Folded: false")
    (Player.print_player b)

let suite =
  "Test Suite"
  >::: [
         "test_compare1" >:: test_compare1;
         "test_compare2" >:: test_compare2;
         "test_compare3" >:: test_compare3;
         "test_compare4" >:: test_compare4;
         "test_compare5" >:: test_compare5;
         "test_compare6" >:: test_compare6;
         "test_compare7" >:: test_compare7;
         "test_compare8" >:: test_compare8;
         "test_compare9" >:: test_compare9;
         "test_compare10" >:: test_compare10;
         "test_compare11" >:: test_compare11;
         "test_compare12" >:: test_compare12;
         "test_compare13" >:: test_compare13;
         "test_compare14" >:: test_compare14;
         "test_string_of_card1" >:: test_string_of_card1;
         "test_string_of_card2" >:: test_string_of_card2;
         "test_string_of_card3" >:: test_string_of_card3;
         "test_string_of_card4" >:: test_string_of_card4;
         "test_string_of_card5" >:: test_string_of_card5;
         "test_string_of_card6" >:: test_string_of_card6;
         "test_string_of_card7" >:: test_string_of_card7;
         "test_string_of_card8" >:: test_string_of_card8;
         "test_string_of_card9" >:: test_string_of_card9;
         "test_string_of_card10" >:: test_string_of_card10;
         "test_string_of_card11" >:: test_string_of_card11;
         "test_string_of_card12" >:: test_string_of_card12;
         "test_string_of_card13" >:: test_string_of_card13;
         "test_string_of_card14" >:: test_string_of_card14;
         "test_string_of_card15" >:: test_string_of_card15;
         "test_string_of_card16" >:: test_string_of_card16;
         "test_string_of_card17" >:: test_string_of_card17;
         "test_string_of_card18" >:: test_string_of_card18;
         "test_string_of_card19" >:: test_string_of_card19;
         "test_string_of_card20" >:: test_string_of_card20;
         "test_string_of_card21" >:: test_string_of_card21;
         "test_string_of_card22" >:: test_string_of_card22;
         "test_string_of_card23" >:: test_string_of_card23;
         "test_string_of_card24" >:: test_string_of_card24;
         "test_string_of_card25" >:: test_string_of_card25;
         "test_string_of_card26" >:: test_string_of_card26;
         "test_string_of_card27" >:: test_string_of_card27;
         "test_string_of_card28" >:: test_string_of_card28;
         "test_string_of_card29" >:: test_string_of_card29;
         "test_string_of_card30" >:: test_string_of_card30;
         "test_string_of_card31" >:: test_string_of_card31;
         "test_string_of_card32" >:: test_string_of_card32;
         "test_string_of_card33" >:: test_string_of_card33;
         "test_string_of_card34" >:: test_string_of_card34;
         "test_string_of_card35" >:: test_string_of_card35;
         "test_string_of_card36" >:: test_string_of_card36;
         "test_string_of_card37" >:: test_string_of_card37;
         "test_string_of_card38" >:: test_string_of_card38;
         "test_string_of_card39" >:: test_string_of_card39;
         "test_string_of_card40" >:: test_string_of_card40;
         "test_string_of_card41" >:: test_string_of_card41;
         "test_string_of_card42" >:: test_string_of_card42;
         "test_string_of_card43" >:: test_string_of_card43;
         "test_string_of_card44" >:: test_string_of_card44;
         "test_string_of_card45" >:: test_string_of_card45;
         "test_string_of_card46" >:: test_string_of_card46;
         "test_string_of_card47" >:: test_string_of_card47;
         "test_string_of_card48" >:: test_string_of_card48;
         "test_string_of_card49" >:: test_string_of_card49;
         "test_string_of_card50" >:: test_string_of_card50;
         "test_string_of_card51" >:: test_string_of_card51;
         "test_string_of_card52" >:: test_string_of_card52;
         "test_hand_add" >:: test_hand_add;
         "test_string_of_hand" >:: test_string_of_hand;
         (* "test_filt_for_straight" >:: test_filt_for_straight; *)
         "test_straight_flush" >:: test_check_straight_flush;
         "test_new_user" >:: test_new_user;
         "test_new_bot" >:: test_new_bot;
         "test_none_player" >:: test_none_player;
         "test_subtract_chips" >:: test_subtract_chips;
         "test_add_chips" >:: test_add_chips;
         "test_deal_card" >:: test_deal_card;
         "test_fold" >:: test_fold;
         "test_print_player" >:: test_print_player;
       ]

let _ = run_test_tt_main suite
