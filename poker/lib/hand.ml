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

(** filters a 7 or 5 card hand such that only cards of the most common suit
    remain *)
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
let sort_by_rank (hnd : t) : t = List.sort Card.compare hnd

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

(* filters a hand for the best straight in the hand, not sensitive to suit *)
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
          | 0 ->
              Some
                [
                  { suit = s1; rank = r1 };
                  { suit = s2; rank = r2 };
                  { suit = s3; rank = r3 };
                  { suit = s4; rank = r4 };
                  { suit = s5; rank = r5 };
                ]
          | 1 ->
              Some
                [
                  { suit = s2; rank = r2 };
                  { suit = s3; rank = r3 };
                  { suit = s4; rank = r4 };
                  { suit = s5; rank = r5 };
                  { suit = s6; rank = r6 };
                ]
          | 2 ->
              Some
                [
                  { suit = s3; rank = r3 };
                  { suit = s4; rank = r4 };
                  { suit = s5; rank = r5 };
                  { suit = s6; rank = r6 };
                  { suit = s7; rank = r7 };
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
          | _ -> failwith "check_four_of_a_kind")
      | None -> None)
  | _ -> None

(** checks for pairs in hnd beginning at index i of hnd |> sort_by_rank. Will
    either be used on 7 or 5 card hands searching for 1 or 2 pair, or 4 card
    hands to be part of a full house. Returns an option, None if no pairs or
    otherwise Some (i, h) where h is a sorted hand containing a pair and i is
    the index of the first of the 2.*)
let check_two (hnd : t) : (int * t) option =
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
      let idx = List.find_opt (fun x -> if x = 2 then true else false) counts in
      match idx with
      | Some i -> (
          match i with
          | 0 | 1 | 2 | 3 | 4 | 5 ->
              Some
                ( i,
                  [
                    { suit = s1; rank = r1 };
                    { suit = s2; rank = r2 };
                    { suit = s3; rank = r3 };
                    { suit = s4; rank = r4 };
                    { suit = s5; rank = r5 };
                    { suit = s6; rank = r6 };
                    { suit = s7; rank = r7 };
                  ] )
          | _ -> failwith "check_two")
      | None -> None)
  | [
   { suit = s1; rank = r1 };
   { suit = s2; rank = r2 };
   { suit = s3; rank = r3 };
   { suit = s4; rank = r4 };
   { suit = s5; rank = r5 };
  ] -> (
      let ranks = List.map Card.int_of_rank [ r1; r2; r3; r4; r5 ] in
      let counts = List.map ((fun lst x -> count lst x) ranks) ranks in
      let idx = List.find_opt (fun x -> if x = 2 then true else false) counts in
      match idx with
      | Some i -> (
          match i with
          | 0 | 1 | 2 | 3 ->
              Some
                ( i,
                  [
                    { suit = s1; rank = r1 };
                    { suit = s2; rank = r2 };
                    { suit = s3; rank = r3 };
                    { suit = s4; rank = r4 };
                    { suit = s5; rank = r5 };
                  ] )
          | _ -> failwith "check_two")
      | None -> None)
  | [
   { suit = s1; rank = r1 };
   { suit = s2; rank = r2 };
   { suit = s3; rank = r3 };
   { suit = s4; rank = r4 };
  ] -> (
      let ranks = List.map Card.int_of_rank [ r1; r2; r3; r4 ] in
      let counts = List.map ((fun lst x -> count lst x) ranks) ranks in
      let idx = List.find_opt (fun x -> if x = 2 then true else false) counts in
      match idx with
      | Some i -> (
          match i with
          | 0 | 1 | 2 ->
              Some
                ( i,
                  [
                    { suit = s1; rank = r1 };
                    { suit = s2; rank = r2 };
                    { suit = s3; rank = r3 };
                    { suit = s4; rank = r4 };
                  ] )
          | _ -> failwith "check_two")
      | None -> None)
  | _ -> None

(** returns a option: None if no 3 of kind exists in hnd or otherwise Some (i,
    h) where h is a sorted hand containing 3 of a kind and i is the index of the
    first of the three. *)
let check_three (hnd : t) : (int * t) option =
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
      let idx = List.find_opt (fun x -> if x = 3 then true else false) counts in
      match idx with
      | Some i -> (
          match i with
          | 0 | 1 | 2 | 3 | 4 ->
              Some
                ( i,
                  [
                    { suit = s1; rank = r1 };
                    { suit = s2; rank = r2 };
                    { suit = s3; rank = r3 };
                    { suit = s4; rank = r4 };
                    { suit = s5; rank = r5 };
                    { suit = s6; rank = r6 };
                    { suit = s7; rank = r7 };
                  ] )
          | _ -> None)
      | None -> None)
  | _ -> None

(** removes elements from a list in the range of indices [i1, i2] *)
let remove_between i1 i2 lst =
  let rec rmb ls n1 n2 idx =
    match ls with
    | h :: t ->
        if idx >= n1 && idx <= n2 then rmb t n1 n2 (idx + 1)
        else h :: rmb t n1 n2 (idx + 1)
    | [] -> []
  in
  rmb lst i1 i2 0

let keep_between i1 i2 lst =
  let rec kbt ls n1 n2 idx =
    match ls with
    | h :: t ->
        if idx >= n1 && idx <= n2 then h :: kbt t n1 n2 (idx + 1)
        else kbt t n1 n2 (idx + 1)
    | [] -> []
  in
  kbt lst i1 i2 0

let check_full_house (hnd : t) : t option =
  match hnd |> check_three with
  | Some (i1, h1) -> (
      match remove_between i1 (i1 + 2) h1 |> check_two with
      | Some (i2, h2) -> (
          let higher_pair = remove_between i2 (i2 + 1) h2 |> check_two in
          match higher_pair with
          | Some (_, h3) -> Some (h1 @ h3)
          | None -> Some (h1 @ h2))
      | None -> None)
  | None -> None

(** returns the last 5 elements of lst*)
let last5 lst =
  let rec l5 ls i =
    match ls with
    | h :: t -> if i > 5 then l5 t (i - 1) else h :: l5 t i
    | [] -> []
  in
  l5 lst (List.length lst)

let check_flush (hnd : t) : t option =
  let flushed = hnd |> filt_for_flush in
  if List.length flushed < 5 then None
  else
    let sorted = flushed |> sort_by_rank in
    Some (last5 sorted)

let check_straight (hnd : t) : t option =
  let straightened = hnd |> filt_for_straight in
  match straightened with
  | Some h -> Some h
  | None -> None

let get_high_card (hnd : t) : Card.t =
  let rec ghc (cds : t) (c : Card.t option) =
    match cds with
    | h :: t -> (
        Card.(
          match h with
          | { suit = _; rank = rh } -> (
              match c with
              | Some crd -> (
                  match crd with
                  | { suit = _; rank = rc } ->
                      if int_of_rank rh > int_of_rank rc then ghc t (Some h)
                      else ghc t (Some crd))
              | None -> ghc t (Some h))))
    | [] -> c
  in
  match ghc hnd None with
  | Some card -> card
  | None -> failwith "No high card??"

let check_three_of_a_kind (hnd : t) : t option =
  let three = check_three hnd in
  match three with
  | Some (i, h) -> (
      let rest = remove_between i (i + 2) h in
      match rest |> sort_by_rank with
      | h1 :: t -> (
          match t with
          | h2 :: _ -> Some (keep_between i (i + 2) h @ [ h1; h2 ])
          | [] -> failwith "check_three_of_a_kind")
      | [] -> failwith "check_three_of_a_kind")
  | None -> None

let check_two_pair (hnd : t) : t option =
  let two1 = check_two hnd in
  match two1 with
  | Some (i1, h1) -> (
      let rest1 = remove_between i1 (i1 + 1) h1 in
      let two2 = check_two rest1 in
      match two2 with
      | Some (i2, h2) ->
          let rest2 = remove_between i2 (i2 + 1) h2 in
          let hc = get_high_card rest2 in
          Some
            (keep_between i1 (i1 + 1) h1 @ keep_between i2 (i2 + 1) h2 @ [ hc ])
      | None -> None)
  | None -> None

let check_one_pair (hnd : t) : t option =
  let two = check_two hnd in
  match two with
  | Some (i, h) ->
      let rest1 = remove_between i (i + 1) h in
      let hc1 = get_high_card rest1 in
      let rest2 = remove_between 0 1 (rest1 |> sort_by_rank) in
      let hc2 = get_high_card rest2 in
      let rest3 = remove_between 0 1 (rest2 |> sort_by_rank) in
      let hc3 = get_high_card rest3 in
      Some (keep_between i (i + 1) h @ [ hc1; hc2; hc3 ])
  | None -> None

let get_high_card_hand (hnd : t) : t = hnd |> sort_by_rank |> keep_between 0 4

let bestofseven (hnd : t) =
  match hnd with
  | _ -> () (* unimplemented *)

let _ = bestofseven
let _ = high_card
