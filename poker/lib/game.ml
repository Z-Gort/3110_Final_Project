type t = {
  players : (int * Hand.t) list;
  deck : Card.t list;
  flop : Card.t list;
  pot : int;
}

(* Represents a brand new deck of cards *)
let newdeck : Card.t list =
  [
    { suit = Clubs; rank = Two };
    { suit = Clubs; rank = Three };
    { suit = Clubs; rank = Four };
    { suit = Clubs; rank = Five };
    { suit = Clubs; rank = Six };
    { suit = Clubs; rank = Seven };
    { suit = Clubs; rank = Eight };
    { suit = Clubs; rank = Nine };
    { suit = Clubs; rank = Ten };
    { suit = Clubs; rank = Jack };
    { suit = Clubs; rank = Queen };
    { suit = Clubs; rank = King };
    { suit = Clubs; rank = Ace };
    { suit = Hearts; rank = Two };
    { suit = Hearts; rank = Three };
    { suit = Hearts; rank = Four };
    { suit = Hearts; rank = Five };
    { suit = Hearts; rank = Six };
    { suit = Hearts; rank = Seven };
    { suit = Hearts; rank = Eight };
    { suit = Hearts; rank = Nine };
    { suit = Hearts; rank = Ten };
    { suit = Hearts; rank = Jack };
    { suit = Hearts; rank = Queen };
    { suit = Hearts; rank = King };
    { suit = Hearts; rank = Ace };
    { suit = Spades; rank = Two };
    { suit = Spades; rank = Three };
    { suit = Spades; rank = Four };
    { suit = Spades; rank = Five };
    { suit = Spades; rank = Six };
    { suit = Spades; rank = Seven };
    { suit = Spades; rank = Eight };
    { suit = Spades; rank = Nine };
    { suit = Spades; rank = Ten };
    { suit = Spades; rank = Jack };
    { suit = Spades; rank = Queen };
    { suit = Spades; rank = King };
    { suit = Spades; rank = Ace };
    { suit = Diamonds; rank = Two };
    { suit = Diamonds; rank = Three };
    { suit = Diamonds; rank = Four };
    { suit = Diamonds; rank = Five };
    { suit = Diamonds; rank = Six };
    { suit = Diamonds; rank = Seven };
    { suit = Diamonds; rank = Eight };
    { suit = Diamonds; rank = Nine };
    { suit = Diamonds; rank = Ten };
    { suit = Diamonds; rank = Jack };
    { suit = Diamonds; rank = Queen };
    { suit = Diamonds; rank = King };
    { suit = Diamonds; rank = Ace };
  ]

let () = Random.self_init ()

(** [rem_card n d i] is the deck [d] with the card [n] cards away from index [i]
    removed. *)
let rec rem_card n d i =
  match d with
  | h :: t -> if i = n then t else h :: rem_card n t (i + 1)
  | [] -> []

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
  let nextd1 = rem_card i1 d 0 in
  let nextd2 = rem_card i2 nextd1 0 in
  let nextd3 = rem_card i3 nextd2 0 in
  let nextd4 = rem_card i4 nextd3 0 in
  let nextd5 = rem_card i5 nextd4 0 in
  let nextdeck = rem_card i6 nextd5 0 in
  (c1, c2, c3, c4, c5, c6, nextdeck)

(** [deal_cards g] pulls 6 cards out of [g.deck] and adds them to the hands of
    the players represented by [g.players] *)
let deal_cards g =
  match g.players with
  | [ (p1, h1); (p2, h2); (p3, h3); (p4, h4); (p5, h5); (p6, h6) ] -> (
      match new_cards g.deck with
      | c1, c2, c3, c4, c5, c6, d ->
          {
            players =
              [
                (p1, Hand.add c1 h1);
                (p2, Hand.add c2 h2);
                (p3, Hand.add c3 h3);
                (p4, Hand.add c4 h4);
                (p5, Hand.add c5 h5);
                (p6, Hand.add c6 h6);
              ];
            deck = d;
            flop = [];
            pot = 0;
          })
  | _ -> g

let emptygame =
  {
    players =
      [
        (1, Hand.empty);
        (2, Hand.empty);
        (3, Hand.empty);
        (4, Hand.empty);
        (5, Hand.empty);
        (6, Hand.empty);
      ];
    deck = newdeck;
    flop = [];
    pot = 0;
  }

let newgame = deal_cards (deal_cards emptygame)
