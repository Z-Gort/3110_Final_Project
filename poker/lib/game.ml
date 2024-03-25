type t = {
  players : int list;
  deck : Card.t list;
  hands : Hand.t list;
}

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

(* let deal_round *)

let newgame () = { players = [ 1; 2; 3; 4; 5; 6 ]; deck = newdeck; hands = [] }
(* unimplemented *)
