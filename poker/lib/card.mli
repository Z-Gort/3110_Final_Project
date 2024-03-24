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
  rank : int;
}
(** Represents a card with a [suit] and a [rank] *)

val compare : t -> t -> int
(** [compare x y] returns a positive integer if [x] > [y], a negative integer if
    [x] < [y], and zero if [x] = [y]. *)
