type t = {
  players : int list;
  deck : Card.t list;
  hands : Hand.t list;
}
(** Represents a game with (6) players with a list of integers corresponding to
    the number of the player in the order that they will be dealt cards (game
    begins with player 1 being dealt a card first), a deck, and the hands of the
    players (order of hands corresponds with order of players) *)

val newgame : unit -> t
(** [newgame ()] is a brand new game only *)

val deal_cards : t -> t
(** [deal_cards g] is the game g but with a cards dealt off the top of the deck
    until *)
