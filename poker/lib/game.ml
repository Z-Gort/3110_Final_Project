type t =
  { players : Player.t list
  ; deck : Card.t list
  ; flop : Card.t list
  ; pot : int
  }

(* Represents a brand new deck of cards *)
let newdeck : Card.t list =
  [ { suit = Clubs; rank = Two }
  ; { suit = Clubs; rank = Three }
  ; { suit = Clubs; rank = Four }
  ; { suit = Clubs; rank = Five }
  ; { suit = Clubs; rank = Six }
  ; { suit = Clubs; rank = Seven }
  ; { suit = Clubs; rank = Eight }
  ; { suit = Clubs; rank = Nine }
  ; { suit = Clubs; rank = Ten }
  ; { suit = Clubs; rank = Jack }
  ; { suit = Clubs; rank = Queen }
  ; { suit = Clubs; rank = King }
  ; { suit = Clubs; rank = Ace }
  ; { suit = Hearts; rank = Two }
  ; { suit = Hearts; rank = Three }
  ; { suit = Hearts; rank = Four }
  ; { suit = Hearts; rank = Five }
  ; { suit = Hearts; rank = Six }
  ; { suit = Hearts; rank = Seven }
  ; { suit = Hearts; rank = Eight }
  ; { suit = Hearts; rank = Nine }
  ; { suit = Hearts; rank = Ten }
  ; { suit = Hearts; rank = Jack }
  ; { suit = Hearts; rank = Queen }
  ; { suit = Hearts; rank = King }
  ; { suit = Hearts; rank = Ace }
  ; { suit = Spades; rank = Two }
  ; { suit = Spades; rank = Three }
  ; { suit = Spades; rank = Four }
  ; { suit = Spades; rank = Five }
  ; { suit = Spades; rank = Six }
  ; { suit = Spades; rank = Seven }
  ; { suit = Spades; rank = Eight }
  ; { suit = Spades; rank = Nine }
  ; { suit = Spades; rank = Ten }
  ; { suit = Spades; rank = Jack }
  ; { suit = Spades; rank = Queen }
  ; { suit = Spades; rank = King }
  ; { suit = Spades; rank = Ace }
  ; { suit = Diamonds; rank = Two }
  ; { suit = Diamonds; rank = Three }
  ; { suit = Diamonds; rank = Four }
  ; { suit = Diamonds; rank = Five }
  ; { suit = Diamonds; rank = Six }
  ; { suit = Diamonds; rank = Seven }
  ; { suit = Diamonds; rank = Eight }
  ; { suit = Diamonds; rank = Nine }
  ; { suit = Diamonds; rank = Ten }
  ; { suit = Diamonds; rank = Jack }
  ; { suit = Diamonds; rank = Queen }
  ; { suit = Diamonds; rank = King }
  ; { suit = Diamonds; rank = Ace }
  ]
;;

let () = Random.self_init ()

(** [rem_card n d i] is the deck [d] with the card at index removed. *)
let rem_card n d =
  let rec rc n d i =
    match d with
    | h :: t -> if i = n then t else h :: rc n t (i + 1)
    | [] -> []
  in
  rc n d 0
;;

(** [new_cards d] pulls 6 cards randomly out of the deck [d] (to simulate
    drawing from the top of a shuffled deck) and returns those cards as the
    first 6 entries in a 7-tuple and the 7th entry as the [d] after those cards
    are removed *)
let new_cards d =
  let decksize = List.length d in
  let i1 = Random.int decksize in
  let i2 = Random.int (decksize - 1) in
  let i3 = Random.int (decksize - 2) in
  let i4 = Random.int (decksize - 3) in
  let i5 = Random.int (decksize - 4) in
  let i6 = Random.int (decksize - 5) in
  let c1 = List.nth d i1 in
  let c2 = List.nth d i2 in
  let c3 = List.nth d i3 in
  let c4 = List.nth d i4 in
  let c5 = List.nth d i5 in
  let c6 = List.nth d i6 in
  let nextd1 = rem_card i1 d in
  let nextd2 = rem_card i2 nextd1 in
  let nextd3 = rem_card i3 nextd2 in
  let nextd4 = rem_card i4 nextd3 in
  let nextd5 = rem_card i5 nextd4 in
  let nextdeck = rem_card i6 nextd5 in
  c1, c2, c3, c4, c5, c6, nextdeck
;;

(** [deal_cards g] pulls 6 cards out of [g.deck] and adds them to the hands of
    the players represented by [g.players] *)
let deal_cards g =
  match g.players with
  | [ p1; p2; p3; p4; p5; p6 ] ->
    (match new_cards g.deck with
     | c1, c2, c3, c4, c5, c6, d ->
       { players =
           [ Player.deal_card p1 c1
           ; Player.deal_card p2 c2
           ; Player.deal_card p3 c3
           ; Player.deal_card p4 c4
           ; Player.deal_card p5 c5
           ; Player.deal_card p6 c6
           ]
       ; deck = d
       ; flop = []
       ; pot = 0
       })
  | _ -> g
;;

let emptygame =
  { players =
      [ Player.new_user
      ; Player.new_bot 1
      ; Player.new_bot 1
      ; Player.new_bot 1
      ; Player.new_bot 1
      ; Player.new_bot 1
      ]
  ; deck = newdeck
  ; flop = []
  ; pot = 0
  }
;;

(*
   let create_bet_list num = num :: [ 10; 10; 10; 10; 10 ]
*)

let rec bet_round game ordered_bets =
  match game.players, ordered_bets with
  | [], _ -> game
  | _, [] -> game
  | h_players :: t_players, h_bets :: t_bets ->
    if h_bets = -1
    then bet_round { game with players = t_players } t_bets
    else
      { game with
        players =
          Player.subtract_chips h_players h_bets
          :: (bet_round { game with players = t_players } t_bets).players
      }
;;

let newgame = deal_cards (deal_cards emptygame)
