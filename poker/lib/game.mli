type t = {
  players : Player.t list;
  deck : Card.t list;
  flop : Card.t list;
  pot : int;
  current_bet : int;
}

(** Represents a game with (6) players with a list of integers corresponding to
    the number of the player in the order that they will be dealt cards (game
    begins with player 1 being dealt a card first), a deck, the flop (cards
    shared among players), and the pot*)

type action =
  | Check
  | Call
  | Raise of int
  | Fold
      (** Represents the action a player can make during the betting round *)

val newgame : t
(** [newgame] is a brand new game with two cards dealt to every player*)

val player_bet : t -> Player.t -> int -> t
(** [player_bet gm p n] is the game with player [p] having bet [n]. *)

val bet_round : t -> int list -> t
