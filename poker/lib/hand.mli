type t
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
