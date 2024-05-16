type t = Card.t list
(** Represents the hand of a player, the two cards they are dealt plus the
    community cards on the flop. *)

val empty : t
(** Represents an empty hand *)

val add : Card.t -> t -> t
(** [add c h] adds card [c] to hand [h]*)

val compare : t -> t -> int
(** [compare x y] returns a positive integer if [x] > [y], a negative integer if
    [x] < [y], and zero if [x] = [y]. *)

val string_of_hand : t -> string
(** [string_of_hand h] is an easily readable string representation of hand [h] *)

val check_straight_flush : t -> t option
(** [check_straight_flush hnd] is either [Some h] where [h] is a 5 card hand
    containing a straight flush made up of cards in [hnd] or [None] if no such
    hand can be made from [hnd]. *)

val eval_hand : t -> string * t
(** [eval_hand hnd] is a tuple whose first element is a string representing the
    best 5 card hand of the input 7 card hand and whose second element is that
    best 5 card hand *)
