type suit =
  | Clubs
  | Spades
  | Hearts
  | Diamonds  (** Represents the suit of a card*)

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
      (** Represents the rank of a card, 2 to 10 followed by jack, queen, king,
          ace, ordered from low to high. *)

type t = {
  suit : suit;
  rank : rank;
}
(** Represents a card with a [suit] and a [rank] *)

val compare : t -> t -> int
(** [compare x y] returns a positive integer if [x] > [y], a negative integer if
    [x] < [y], and zero if [x] = [y]. *)

val int_of_rank : rank -> int
(** [int_of_rank r] converts [r] in to an integer from 2 to 14 for the purpose
    of comparing ranks more easily *)

val rank_of_int : int -> rank
(** [rank_of_int (int_of_rank r)] = [r] *)

val string_of_card : t -> string
(** [string_of_card c] is an easily readable string representation of card [c]*)
