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
    shared among players), and the pot. Additionally, the current minimum bet
    (current_bet), the last player to raise (last_raise), the chips the player
    had at the start of the round (round_chips), and the total bet on the round
    are stored. *)

val newgame : t
(** [newgame] is a brand new game*)

val deal_cards : t -> t
(** [deal_cards g] is to be used at the beginning of rounds, it deals a new card
    to the hand of each player in [g.players]*)

val player_bet : t -> Player.t -> int -> t
(** [player_bet gm p n] is the the same as the game [gm] but with with player
    [p] having bet [n]. If [p] raises, [gm.last_raise] is updated to be [p]. *)

val fold_player : t -> Player.t -> t
(** [fold_player gm pl] is [gm] after [pl] has folded *)

val print_game : t -> unit
(** prnts the current [players] and [pot] *)

val deal_flop : t -> t
(** modifies game to have 3 random cards flopped, modifies deck accordingly,
    resets betting fields*)

val deal_turn : t -> t
(** adds a turn card to our game, modifies deck accordingly, resets betting
    fields*)

val deal_river : t -> t
(** adds a river card to our game, modifies deck accordingly, resets betting
    fields*)

val plist_to_string : Player.t list -> string
(** converts a list of players to a string *)

val pick_round_winner : t -> t
(** picks the winner of the round and returns a new game after that player has
    won*)
