type t = {
  players : (int * Hand.t) list;
  deck : Card.t list;
}
(** Represents a game with (6) players with a list of integers corresponding to
    the number of the player in the order that they will be dealt cards (game
    begins with player 1 being dealt a card first), a deck, and the hands of the
    players (order of hands corresponds with order of players) *)

val newgame : t
(** [newgame] is a brand new game with no cards yet dealt *)

val deal_cards : t -> t
(** [deal_cards g] is the game g but with cards dealt off the top of the deck
    until each player has recieved a new card*)
