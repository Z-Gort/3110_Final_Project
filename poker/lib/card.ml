type suit =
  | Clubs
  | Spades
  | Hearts
  | Diamonds

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

type t = {
  suit : suit;
  rank : rank;
}

let int_of_rank = function
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14

let rank_of_int = function
  | 2 -> Two
  | 3 -> Three
  | 4 -> Four
  | 5 -> Five
  | 6 -> Six
  | 7 -> Seven
  | 8 -> Eight
  | 9 -> Nine
  | 10 -> Ten
  | 11 -> Jack
  | 12 -> Queen
  | 13 -> King
  | 14 -> Ace
  | _ -> failwith "rank_of_int received number not in the range [2,14]"

let compare c1 c2 = Int.compare (int_of_rank c1.rank) (int_of_rank c2.rank)

let string_of_card c =
  let st =
    (fun x ->
      match x.suit with
      | Clubs -> "Clubs"
      | Spades -> "Spades"
      | Hearts -> "Hearts"
      | Diamonds -> "Diamonds")
      c
  in
  let rnk =
    (fun x ->
      match x.rank with
      | Two -> "Two"
      | Three -> "Three"
      | Four -> "Four"
      | Five -> "Five"
      | Six -> "Six"
      | Seven -> "Seven"
      | Eight -> "Eight"
      | Nine -> "Nine"
      | Ten -> "Ten"
      | Jack -> "Jack"
      | Queen -> "Queen"
      | King -> "King"
      | Ace -> "Ace")
      c
  in
  rnk ^ " of " ^ st
