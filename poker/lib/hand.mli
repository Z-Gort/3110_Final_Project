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

val check_four_of_a_kind : t -> t option
(** [check_four_of_a_kind hnd] is either [Some h] where [h] is a 5 card hand
    containing 4 of a kind from [hnd] or [None] if no such hand can be made from
    [hnd]. *)

val check_full_house : t -> t option
(** [check_full_house hnd] is either [Some h] where [h] is a 5 card hand
    containing a full house or [None] if no such hand can be made from [hnd]. *)

val check_flush : t -> t option
(** [check_flush hnd] is either [Some h] where [h] is a 5 card hand containing a
    flush or [None] if no such hand can be made from [hnd]*)

val check_straight : t -> t option
(** [check_straight hnd] is either [Some h] where [h] is a 5 card hand
    containing a straigh or [None] if no such hand can be made from [hnd]*)

val check_three_of_a_kind : t -> t option
(** [check_three hnd] is either [Some h] where [h] is a 5 card hand containing 3
    of a kind from [hnd] or [None] if no such hand can be made from [hnd]. *)

val check_two_pair : t -> t option
val check_one_pair : t -> t option
val get_high_card_hand : t -> t
val get_high_card : t -> Card.t
