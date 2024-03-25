type t = {
  players : int list;
  deck : Card.t list;
  hands : int list;
}
(** Represents a game with players represented as a list of integers
    corresponding to the number of the player (dealing starts with player 1 at
    the beginning of the game), a deck *)
