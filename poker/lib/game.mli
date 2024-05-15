type t = {
  players : Player.t list;
  deck : Card.t list;
  flop_turn_river : Card.t list;
  pot : int;
  current_bet : int;
  last_raise : Player.t;
  round_chips : int;
  total_bet : int;
}

(** Represents a game with (6) players with a list of integers corresponding to
    the number of the player in the order that they will be dealt cards (game
    begins with player 1 being dealt a card first), a deck, the flop (cards
    shared among players), and the pot*)

val newgame : t
(** [newgame] is a brand new game with two cards dealt to every player*)

val player_bet : t -> Player.t -> int -> t
(** [player_bet gm p n] is the the same as the game [gm] but with with player
    [p] having bet [n]. If [p] raises, [gm.last_raise] is updated to be [p]. *)

val fold_player : t -> Player.t -> t
(** [fold_player gm pl] is [gm] after [pl] has folded *)

val print_game : t -> unit
(** prnts the current [players] and [pot] *)

val deal_flop : t -> t
(**modifies game to have 3 random cards flopped, modifies deck accordingly, resets betting fields*)

val deal_turn : t -> t
(**adds a turn card to our game, modifies deck accordingly, resets betting fields*)

val deal_river : t -> t
(**adds a river card to our game, modifies deck accordingly, resets betting fields*)



(* val bet_round : t -> int list -> t *)
