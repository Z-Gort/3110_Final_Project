type ptype =
  | User
  | Bot of int
      (** [ptype] is the type that represents a player type, distinguishing bots
          from the user. Bot carries an int to represent which betting logit it
          uses. *)

type t = {
  player_type : ptype;
  hand : Hand.t;
  chips : int;
}
(** Player.t represents a poker player with a [player_type] that represents
    their status as either a particular bot or the user, a [hand] representing
    their hand, and [chips] which represents how many chips they can use to bet
    with. *)
