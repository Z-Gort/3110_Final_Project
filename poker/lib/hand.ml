type t = Card.t list
(** AF: a hand is represented by a list of cards in any order. RI: all cards in
    a hand are different and the size of a hand can never be greater than 5
    cards *)

let empty = []

let add c lst =
  match lst with
  | [] -> c :: []
  | v -> v @ [ c ]

let compare h1 h2 =
  match (h1, h2) with
  | _ -> 0
(* currently treats all hands as equal, needs to be fully implemented *)

let string_of_hand hnd =
  let rec build str lst =
    match lst with
    | [] -> str
    | [ h1; h2 ] -> str ^ Card.string_of_card h1 ^ ", " ^ Card.string_of_card h2
    | h :: t -> build (str ^ Card.string_of_card h ^ ", ") t
  in
  build "" hnd

let count lst elem =
  List.fold_left (fun acc x -> if x = elem then acc + 1 else acc) 0 lst

(** Only to be applied to 5 card hands *)
let high_card (hnd : t) : Card.t =
  match hnd with
  | [
   { suit = s1; rank = r1 };
   { suit = s2; rank = r2 };
   { suit = s3; rank = r3 };
   { suit = s4; rank = r4 };
   { suit = s5; rank = r5 };
  ] ->
      let rnks = [ r1; r2; r3; r4; r5 ] in
      let intranks = List.map Card.int_of_rank rnks in
      let max_rank =
        Card.rank_of_int
          (List.fold_left (fun acc x -> if x > acc then x else acc) 0 intranks)
      in
      if max_rank = r1 then { suit = s1; rank = r1 }
      else if max_rank = r2 then { suit = s2; rank = r2 }
      else if max_rank = r3 then { suit = s3; rank = r3 }
      else if max_rank = r4 then { suit = s4; rank = r4 }
      else if max_rank = r5 then { suit = s5; rank = r5 }
      else failwith "this should not happen in high_card"
  | _ -> failwith "high_card should only be applied to 5 card hands"

(** filters a 7 card hand such that only cards of the most common suit remain *)
let filt_for_suit (hnd : t) =
  match hnd with
  | [
   { suit = s1; rank = _ };
   { suit = s2; rank = _ };
   { suit = s3; rank = _ };
   { suit = s4; rank = _ };
   { suit = s5; rank = _ };
   { suit = s6; rank = _ };
   { suit = s7; rank = _ };
  ] ->
      Card.(
        let suits = [ s1; s2; s3; s4; s5; s6; s7 ] in
        let counts =
          [
            (Clubs, count suits Clubs);
            (Spades, count suits Spades);
            (Hearts, count suits Hearts);
            (Diamonds, count suits Diamonds);
          ]
        in
        let most_suit_count =
          List.fold_left
            (fun acc x -> if snd x > snd acc then x else acc)
            (Clubs, 0) counts
        in
        let most_suit = fst most_suit_count in
        let flush_hand =
          List.fold_left
            (fun acc x -> if x.suit = most_suit then acc @ [ x ] else acc)
            [] hnd
        in
        flush_hand)
  | [
   { suit = s1; rank = _ };
   { suit = s2; rank = _ };
   { suit = s3; rank = _ };
   { suit = s4; rank = _ };
   { suit = s5; rank = _ };
  ] ->
      Card.(
        let suits = [ s1; s2; s3; s4; s5 ] in
        let counts =
          [
            (Clubs, count suits Clubs);
            (Spades, count suits Spades);
            (Hearts, count suits Hearts);
            (Diamonds, count suits Diamonds);
          ]
        in
        let most_suit_count =
          List.fold_left
            (fun acc x -> if snd x > snd acc then x else acc)
            (Clubs, 0) counts
        in
        let most_suit = fst most_suit_count in
        let flush_hand =
          List.fold_left
            (fun acc x -> if x.suit = most_suit then acc @ [ x ] else acc)
            [] hnd
        in
        flush_hand)
  | [] -> []
  | _ -> failwith "filt_for_suit should only be applied to 5 or 7 card hands"

(* sorts the cards in a hand by rank *)
let sort_by_rank (hnd : t) = List.sort Card.compare hnd
let find_consecutive lst = if lst = lst then [] else []

(* let rec find_five_consecutive lst = match lst with | h :: _ -> ( match lst
   with | ) *)

(* filters a hand for consecutive cards, not sensitive to suit *)
let filt_for_straight (hnd : t) : t option =
  let sorted_hnd = hnd |> sort_by_rank in
  match sorted_hnd with
  | [
   { suit = s1; rank = r1 };
   { suit = s2; rank = r2 };
   { suit = s3; rank = r3 };
   { suit = s4; rank = r4 };
   { suit = s5; rank = r5 };
   { suit = s6; rank = r6 };
   { suit = s7; rank = r7 };
  ] -> (
      Card.(
        let intrnks = List.map int_of_rank [ r1; r2; r3; r4; r5; r6; r7 ] in
        let sequential = find_consecutive intrnks in
        match sequential with
        | [ d1; d2; d3; d4; d5; d6; d7 ] ->
            let crddfs =
              [
                (d1, { suit = s1; rank = r1 });
                (d2, { suit = s2; rank = r2 });
                (d3, { suit = s3; rank = r3 });
                (d4, { suit = s4; rank = r4 });
                (d5, { suit = s5; rank = r5 });
                (d6, { suit = s6; rank = r6 });
                (d7, { suit = s7; rank = r7 });
              ]
            in
            let consecutive =
              List.fold_left
                (fun acc x -> if fst x = 1 then acc @ [ snd x ] else acc)
                [] crddfs
            in

            if List.length consecutive > 4 then Some consecutive
            else Some consecutive
        | _ -> failwith "filt_for_straight"))
  | _ -> failwith "filt_for_straight should only be applied to 7 card hands"

let bestofseven (hnd : t) =
  match hnd with
  | _ -> () (* unimplemented *)

let _ = bestofseven
let _ = high_card
