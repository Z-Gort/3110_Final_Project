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

(** counts occurrences of elem in lst *)
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
let filt_for_flush (hnd : t) : t =
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

(** [make_zeros n] is a list of zeros of length n*)
let make_zeros n =
  let rec mz i =
    match i with
    | 0 -> []
    | nonzero -> 0 :: mz (nonzero - 1)
  in
  mz n

(* detects the first 5 consecutive sequential numbers in a list, returns a 1
   where the member of the original list is part of the sequence and 0 where the
   member of the original list was not*)
let find_five_sequential lst : int list option =
  let rec ffc lst a idx =
    if count a 1 = 5 then a
    else
      match lst with
      | [ h1; h2 ] -> if h1 + 1 = h2 then a @ [ 1 ] else a @ [ 0 ]
      | h1 :: h2 :: t ->
          if h1 + 1 = h2 then ffc (h2 :: t) (a @ [ 1 ]) (idx + 1)
          else ffc (h2 :: t) (make_zeros idx @ [ 1 ]) (idx + 1)
      | _ -> []
  in
  if List.length lst < 5 then None
  else
    match lst with
    | h1 :: h2 :: t ->
        if h2 = h1 + 1 then
          let res = ffc (h2 :: t) [ 1; 1 ] 2 in
          if count res 1 >= 5 then Some res else None
        else
          let res = ffc (h2 :: t) [ 0; 1 ] 2 in
          if count res 1 >= 5 then Some res else None
    | _ -> None

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
        match intrnks with
        | [ i1; i2; i3; i4; i5; i6; i7 ] -> (
            let straights =
              ( find_five_sequential [ i3; i4; i5; i6; i7 ],
                find_five_sequential [ i2; i3; i4; i5; i6 ],
                find_five_sequential [ i1; i2; i3; i4; i5 ] )
            in
            match straights with
            | Some _, _, _ ->
                Some
                  [
                    { suit = s3; rank = r3 };
                    { suit = s4; rank = r4 };
                    { suit = s5; rank = r5 };
                    { suit = s6; rank = r6 };
                    { suit = s7; rank = r7 };
                  ]
            | None, Some _, _ ->
                Some
                  [
                    { suit = s2; rank = r2 };
                    { suit = s3; rank = r3 };
                    { suit = s4; rank = r4 };
                    { suit = s5; rank = r5 };
                    { suit = s6; rank = r6 };
                  ]
            | None, None, Some _ ->
                Some
                  [
                    { suit = s1; rank = r1 };
                    { suit = s2; rank = r2 };
                    { suit = s3; rank = r3 };
                    { suit = s4; rank = r4 };
                    { suit = s5; rank = r5 };
                  ]
            | None, None, None -> None)
        | _ -> failwith "filt_for_straight"))
  | [
   { suit = s1; rank = r1 };
   { suit = s2; rank = r2 };
   { suit = s3; rank = r3 };
   { suit = s4; rank = r4 };
   { suit = s5; rank = r5 };
   { suit = s6; rank = r6 };
  ] -> (
      Card.(
        let intrnks = List.map int_of_rank [ r1; r2; r3; r4; r5; r6 ] in
        match intrnks with
        | [ i1; i2; i3; i4; i5; i6 ] -> (
            let straights =
              ( find_five_sequential [ i2; i3; i4; i5; i6 ],
                find_five_sequential [ i1; i2; i3; i4; i5 ] )
            in
            match straights with
            | Some _, _ ->
                Some
                  [
                    { suit = s2; rank = r2 };
                    { suit = s3; rank = r3 };
                    { suit = s4; rank = r4 };
                    { suit = s5; rank = r5 };
                    { suit = s6; rank = r6 };
                  ]
            | None, Some _ ->
                Some
                  [
                    { suit = s1; rank = r1 };
                    { suit = s2; rank = r2 };
                    { suit = s3; rank = r3 };
                    { suit = s4; rank = r4 };
                    { suit = s5; rank = r5 };
                  ]
            | None, None -> None)
        | _ -> failwith "filt_for_straight"))
  | [
   { suit = s1; rank = r1 };
   { suit = s2; rank = r2 };
   { suit = s3; rank = r3 };
   { suit = s4; rank = r4 };
   { suit = s5; rank = r5 };
  ] -> (
      Card.(
        let intrnks = List.map int_of_rank [ r1; r2; r3; r4; r5 ] in
        match intrnks with
        | [ i1; i2; i3; i4; i5 ] -> (
            let straight = find_five_sequential [ i1; i2; i3; i4; i5 ] in
            match straight with
            | Some _ ->
                Some
                  [
                    { suit = s1; rank = r1 };
                    { suit = s2; rank = r2 };
                    { suit = s3; rank = r3 };
                    { suit = s4; rank = r4 };
                    { suit = s5; rank = r5 };
                  ]
            | None -> None)
        | _ -> failwith "filt_for_straight"))
  | _ -> failwith "filt_for_straight should only be applied to 7 card hands"

let check_straight_flush (hnd : t) : t option =
  let flushed = hnd |> filt_for_flush in
  if List.length flushed >= 5 then filt_for_straight flushed else None

let check_four_of_a_kind (hnd : t) : t option =
  match hnd |> sort_by_rank with
  | [
   { suit = s1; rank = r1 };
   { suit = s2; rank = r2 };
   { suit = s3; rank = r3 };
   { suit = s4; rank = r4 };
   { suit = s5; rank = r5 };
   { suit = s6; rank = r6 };
   { suit = s7; rank = r7 };
  ] -> (
      let ranks = List.map Card.int_of_rank [ r1; r2; r3; r4; r5; r6; r7 ] in
      let counts = List.map ((fun lst x -> count lst x) ranks) ranks in
      let idx = List.find_opt (fun x -> if x = 4 then true else false) counts in
      match idx with
      | Some i -> (
          match i with
          | 1 ->
              Some
                [
                  { suit = s1; rank = r1 };
                  { suit = s2; rank = r2 };
                  { suit = s3; rank = r3 };
                  { suit = s4; rank = r4 };
                  { suit = s5; rank = r5 };
                ]
          | 2 ->
              Some
                [
                  { suit = s2; rank = r2 };
                  { suit = s3; rank = r3 };
                  { suit = s4; rank = r4 };
                  { suit = s5; rank = r5 };
                  { suit = s6; rank = r6 };
                ]
          | 3 ->
              Some
                [
                  { suit = s3; rank = r3 };
                  { suit = s4; rank = r4 };
                  { suit = s5; rank = r5 };
                  { suit = s6; rank = r6 };
                  { suit = s7; rank = r7 };
                ]
          | 4 ->
              Some
                [
                  { suit = s3; rank = r3 };
                  { suit = s4; rank = r4 };
                  { suit = s5; rank = r5 };
                  { suit = s6; rank = r6 };
                  { suit = s7; rank = r7 };
                ]
          | _ -> failwith "check_four_of_a_kind")
      | None -> None)
  | _ -> None

let bestofseven (hnd : t) =
  match hnd with
  | _ -> () (* unimplemented *)

let _ = bestofseven
let _ = high_card
