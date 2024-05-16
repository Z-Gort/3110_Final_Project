type t = {
  players : Player.t list;
  deck : Card.t list;
  flop_turn_river : Card.t list;
  pot : int;
  current_bet : int;
  last_raise : Player.t;
  round_chips : int;
  total_bet : int;
}

(* Represents a brand new deck of cards *)
let newdeck : Card.t list =
  [
    { suit = Clubs; rank = Two };
    { suit = Clubs; rank = Three };
    { suit = Clubs; rank = Four };
    { suit = Clubs; rank = Five };
    { suit = Clubs; rank = Six };
    { suit = Clubs; rank = Seven };
    { suit = Clubs; rank = Eight };
    { suit = Clubs; rank = Nine };
    { suit = Clubs; rank = Ten };
    { suit = Clubs; rank = Jack };
    { suit = Clubs; rank = Queen };
    { suit = Clubs; rank = King };
    { suit = Clubs; rank = Ace };
    { suit = Hearts; rank = Two };
    { suit = Hearts; rank = Three };
    { suit = Hearts; rank = Four };
    { suit = Hearts; rank = Five };
    { suit = Hearts; rank = Six };
    { suit = Hearts; rank = Seven };
    { suit = Hearts; rank = Eight };
    { suit = Hearts; rank = Nine };
    { suit = Hearts; rank = Ten };
    { suit = Hearts; rank = Jack };
    { suit = Hearts; rank = Queen };
    { suit = Hearts; rank = King };
    { suit = Hearts; rank = Ace };
    { suit = Spades; rank = Two };
    { suit = Spades; rank = Three };
    { suit = Spades; rank = Four };
    { suit = Spades; rank = Five };
    { suit = Spades; rank = Six };
    { suit = Spades; rank = Seven };
    { suit = Spades; rank = Eight };
    { suit = Spades; rank = Nine };
    { suit = Spades; rank = Ten };
    { suit = Spades; rank = Jack };
    { suit = Spades; rank = Queen };
    { suit = Spades; rank = King };
    { suit = Spades; rank = Ace };
    { suit = Diamonds; rank = Two };
    { suit = Diamonds; rank = Three };
    { suit = Diamonds; rank = Four };
    { suit = Diamonds; rank = Five };
    { suit = Diamonds; rank = Six };
    { suit = Diamonds; rank = Seven };
    { suit = Diamonds; rank = Eight };
    { suit = Diamonds; rank = Nine };
    { suit = Diamonds; rank = Ten };
    { suit = Diamonds; rank = Jack };
    { suit = Diamonds; rank = Queen };
    { suit = Diamonds; rank = King };
    { suit = Diamonds; rank = Ace };
  ]

let () = Random.self_init ()

(** [rem_card n d i] is the deck [d] with the card at index removed. *)
let rem_card n d =
  let rec rc n d i =
    match d with
    | h :: t -> if i = n then t else h :: rc n t (i + 1)
    | [] -> []
  in
  rc n d 0

(** [new_cards d] pulls 6 cards randomly out of the deck [d] (to simulate
    drawing from the top of a shuffled deck) and returns those cards as the
    first 6 entries in a 7-tuple and the 7th entry as the [d] after those cards
    are removed *)
let new_cards d =
  let decksize = List.length d in
  let i1 = Random.int decksize in
  let i2 = Random.int (decksize - 1) in
  let i3 = Random.int (decksize - 2) in
  let i4 = Random.int (decksize - 3) in
  let i5 = Random.int (decksize - 4) in
  let i6 = Random.int (decksize - 5) in
  let c1 = List.nth d i1 in
  let nextd1 = rem_card i1 d in
  let c2 = List.nth nextd1 i2 in
  let nextd2 = rem_card i2 nextd1 in
  let c3 = List.nth nextd2 i3 in
  let nextd3 = rem_card i3 nextd2 in
  let c4 = List.nth nextd3 i4 in
  let nextd4 = rem_card i4 nextd3 in
  let c5 = List.nth nextd4 i5 in
  let nextd5 = rem_card i5 nextd4 in
  let c6 = List.nth nextd5 i6 in
  let nextdeck = rem_card i6 nextd5 in
  (* let nextd1 = rem_card i1 d in let nextd2 = rem_card i2 nextd1 in let nextd3
     = rem_card i3 nextd2 in let nextd4 = rem_card i4 nextd3 in let nextd5 =
     rem_card i5 nextd4 in let nextdeck = rem_card i6 nextd5 in *)
  (c1, c2, c3, c4, c5, c6, nextdeck)

let flop d =
  let decksize = List.length d in
  let i1 = Random.int decksize in
  let i2 = Random.int (decksize - 1) in
  let i3 = Random.int (decksize - 2) in
  let c1 = List.nth d i1 in
  let nextd1 = rem_card i1 d in
  let c2 = List.nth nextd1 i2 in
  let nextd2 = rem_card i2 nextd1 in
  let c3 = List.nth nextd2 i3 in
  let nextdeck = rem_card i3 nextd2 in
  (c1, c2, c3, nextdeck)

let turn_and_river d =
  let decksize = List.length d in
  let i1 = Random.int decksize in
  let c1 = List.nth d i1 in
  let nextdeck = rem_card i1 d in
  (c1, nextdeck)

(**printing made with help of ChatGPT 5/13*)
let deal_turn g =
  match turn_and_river g.deck with
  | c1, d ->
      print_endline "\n*******SECOND ROUND OF BETTING DONE*******\n";

      (* Create a string representation of all cards on the board (flop +
         turn) *)
      let cards_string =
        String.concat ", "
          (List.map Card.string_of_card g.flop_turn_river
          @ [ Card.string_of_card c1 ])
      in
      print_endline ("The flop + turn is: " ^ cards_string);
      print_newline ();
      (fun (x : t) ->
        match x.players with
        | p1 :: _ ->
            print_endline
              ("You currently have " ^ string_of_int p1.chips ^ " chips")
        | _ -> ())
        g;

      if List.length g.players = 0 then (
        print_endline "You all folded—no one wins in this version of poker :(";
        exit 0);

      let round_chps = (List.nth g.players 0).chips in
      let playurs =
        g.players
        |> List.map (fun (p : Player.t) -> { p with hand = Hand.add c1 p.hand })
      in
      {
        g with
        players = playurs;
        flop_turn_river = c1 :: g.flop_turn_river;
        current_bet = 0;
        last_raise = Player.none_player;
        round_chips = round_chps;
        total_bet = 0;
        deck = d;
      }

(**printing made with help of ChatGPT 5/13*)
let deal_river g =
  match turn_and_river g.deck with
  | c1, d ->
      print_endline "\n*******THIRD ROUND OF BETTING DONE*******\n";

      (* Create a string representation of all cards on the board (flop +
         turn) *)
      let cards_string =
        String.concat ", "
          (List.map Card.string_of_card g.flop_turn_river
          @ [ Card.string_of_card c1 ])
      in
      print_endline ("The flop + turn + river is: " ^ cards_string);
      print_newline ();
      (fun (x : t) ->
        match x.players with
        | p1 :: _ ->
            print_endline
              ("You currently have " ^ string_of_int p1.chips ^ " chips")
        | _ -> ())
        g;

      if List.length g.players = 0 then (
        print_endline "You all folded—no one wins in this version of poker :(";
        exit 0);

      let round_chps = (List.nth g.players 0).chips in
      let playurs =
        g.players
        |> List.map (fun (p : Player.t) -> { p with hand = Hand.add c1 p.hand })
      in
      {
        g with
        players = playurs;
        flop_turn_river = c1 :: g.flop_turn_river;
        current_bet = 0;
        last_raise = Player.none_player;
        round_chips = round_chps;
        total_bet = 0;
        deck = d;
      }

let deal_flop g =
  match flop g.deck with
  | c1, c2, c3, d ->
      print_endline "\n*******FIRST ROUND OF BETTING DONE*******\n";
      print_endline
        ("The flop is: " ^ Card.string_of_card c1 ^ ", "
       ^ Card.string_of_card c2 ^ ", " ^ Card.string_of_card c3 ^ "\n");
      (fun (x : t) ->
        match x.players with
        | p1 :: _ ->
            print_endline
              ("You currently have " ^ string_of_int p1.chips ^ " chips")
        | _ -> ())
        g;

      if List.length g.players = 0 then (
        print_endline "You all folded—no one wins in this version of poker :(";
        exit 0);

      let round_chps = (List.nth g.players 0).chips in
      let playurs =
        g.players
        |> List.map (fun (p : Player.t) -> { p with hand = Hand.add c1 p.hand })
        |> List.map (fun (p : Player.t) -> { p with hand = Hand.add c2 p.hand })
        |> List.map (fun (p : Player.t) -> { p with hand = Hand.add c3 p.hand })
      in

      {
        g with
        players = playurs;
        flop_turn_river = [ c1; c2; c3 ];
        deck = d;
        current_bet = 0;
        last_raise = Player.none_player;
        round_chips = round_chps;
        total_bet = 0;
      }

(** [deal_cards g] pulls 6 cards out of [g.deck] and adds them to the hands of
    the players represented by [g.players] *)
let deal_cards g =
  match g.players with
  | [ p1; p2; p3; p4; p5; p6 ] -> (
      match new_cards g.deck with
      | c1, c2, c3, c4, c5, c6, d ->
          {
            players =
              [
                Player.deal_card p1 c1;
                Player.deal_card p2 c2;
                Player.deal_card p3 c3;
                Player.deal_card p4 c4;
                Player.deal_card p5 c5;
                Player.deal_card p6 c6;
              ];
            deck = d;
            flop_turn_river = [];
            pot = 0;
            current_bet = 0;
            last_raise = Player.none_player;
            round_chips = Player.default_chips;
            total_bet = 0;
          })
  | _ -> g

let emptygame =
  {
    players =
      [
        Player.new_user;
        Player.new_bot 1;
        Player.new_bot 2;
        Player.new_bot 3;
        Player.new_bot 4;
        Player.new_bot 5;
      ];
    deck = newdeck;
    flop_turn_river = [];
    pot = 0;
    current_bet = 0;
    last_raise = Player.none_player;
    round_chips = Player.default_chips;
    total_bet = 0;
  }

let player_bet (g : t) (p : Player.t) (b : int) =
  match g.players with
  | [ p1; p2; p3; p4; p5; p6 ] ->
      if p1 = p then
        if b > g.current_bet then
          let bettor = Player.subtract_chips p b in
          {
            g with
            players = [ bettor; p2; p3; p4; p5; p6 ];
            pot = g.pot + b;
            current_bet = b;
            last_raise = bettor;
            total_bet = b + (g.round_chips - p.chips);
          }
        else
          {
            g with
            players = [ Player.subtract_chips p b; p2; p3; p4; p5; p6 ];
            pot = g.pot + b;
            current_bet = b;
            total_bet = b + (g.round_chips - p.chips);
          }
      else if p2 = p then
        if b > g.current_bet then
          let bettor = Player.subtract_chips p b in
          {
            g with
            players = [ p1; bettor; p3; p4; p5; p6 ];
            pot = g.pot + b;
            current_bet = b;
            last_raise = bettor;
            total_bet = b + (g.round_chips - p.chips);
          }
        else
          {
            g with
            players = [ p1; Player.subtract_chips p b; p3; p4; p5; p6 ];
            pot = g.pot + b;
            current_bet = b;
            total_bet = b + (g.round_chips - p.chips);
          }
      else if p3 = p then
        if b > g.current_bet then
          let bettor = Player.subtract_chips p b in
          {
            g with
            players = [ p1; p2; bettor; p4; p5; p6 ];
            pot = g.pot + b;
            current_bet = b;
            last_raise = bettor;
            total_bet = b + (g.round_chips - p.chips);
          }
        else
          {
            g with
            players = [ p1; p2; Player.subtract_chips p b; p4; p5; p6 ];
            pot = g.pot + b;
            current_bet = b;
            total_bet = b + (g.round_chips - p.chips);
          }
      else if p4 = p then
        if b > g.current_bet then
          let bettor = Player.subtract_chips p b in
          {
            g with
            players = [ p1; p2; p3; bettor; p5; p6 ];
            pot = g.pot + b;
            current_bet = b;
            last_raise = bettor;
            total_bet = b + (g.round_chips - p.chips);
          }
        else
          {
            g with
            players = [ p1; p2; p3; Player.subtract_chips p b; p5; p6 ];
            pot = g.pot + b;
            current_bet = b;
            total_bet = b + (g.round_chips - p.chips);
          }
      else if p5 = p then
        if b > g.current_bet then
          let bettor = Player.subtract_chips p b in
          {
            g with
            players = [ p1; p2; p3; p4; bettor; p6 ];
            pot = g.pot + b;
            current_bet = b;
            last_raise = bettor;
            total_bet = b + (g.round_chips - p.chips);
          }
        else
          {
            g with
            players = [ p1; p2; p3; p4; Player.subtract_chips p b; p6 ];
            pot = g.pot + b;
            current_bet = b;
            total_bet = b + (g.round_chips - p.chips);
          }
      else if p6 = p then
        if b > g.current_bet then
          let bettor = Player.subtract_chips p b in
          {
            g with
            players = [ p1; p2; p3; p4; p5; bettor ];
            pot = g.pot + b;
            current_bet = b;
            last_raise = bettor;
            total_bet = b + (g.round_chips - p.chips);
          }
        else
          {
            g with
            players = [ p1; p2; p3; p4; p5; Player.subtract_chips p b ];
            pot = g.pot + b;
            current_bet = b;
            total_bet = b + (g.round_chips - p.chips);
          }
      else
        let _ = print_endline "this is not supposed to happen in player_bet" in
        g
  | _ ->
      let _ = print_endline "this is not supposed to happen in player_bet" in
      g

let fold_player gm plyr =
  match gm.players with
  | [ p1; p2; p3; p4; p5; p6 ] ->
      if p1 = plyr then
        { gm with players = [ Player.fold p1; p2; p3; p4; p5; p6 ] }
      else if p2 = plyr then
        { gm with players = [ p1; Player.fold p2; p3; p4; p5; p6 ] }
      else if p3 = plyr then
        { gm with players = [ p1; p2; Player.fold p3; p4; p5; p6 ] }
      else if p4 = plyr then
        { gm with players = [ p1; p2; p3; Player.fold p4; p5; p6 ] }
      else if p5 = plyr then
        { gm with players = [ p1; p2; p3; p4; Player.fold p5; p6 ] }
      else if p6 = plyr then
        { gm with players = [ p1; p2; p3; p4; p5; Player.fold p6 ] }
      else
        let _ = print_endline "this is not supposed to happen in fold_player" in
        gm
  | _ -> gm

(* doesn't count players who have folded *)
let rec plist_to_string plist =
  match plist with
  | h :: t ->
      if not h.Player.folded then Player.p_to_string h ^ "|" ^ plist_to_string t
      else plist_to_string t
  | [] -> ""

let print_game gm =
  print_endline "Game status:";
  print_endline ("players: " ^ plist_to_string gm.players);
  print_endline ("pot: " ^ string_of_int gm.pot)

let newgame = emptygame

let won_round (gm : t) (plyr : Player.t) =
  match gm.players with
  | [ p1; p2; p3; p4; p5; p6 ] ->
      if p1 = plyr then
        let user_is_up = Player.add_chips p1 gm.pot in
        {
          players =
            List.map
              (fun x -> x |> Player.unfold |> Player.reset_hand)
              [ user_is_up; p2; p3; p4; p5; p6 ];
          deck = newdeck;
          flop_turn_river = [];
          pot = 0;
          current_bet = 0;
          last_raise = Player.none_player;
          round_chips = user_is_up.chips;
          total_bet = 0;
        }
      else if p2 = plyr then
        {
          players =
            List.map
              (fun x -> x |> Player.unfold |> Player.reset_hand)
              [ p1; Player.add_chips p2 gm.pot; p3; p4; p5; p6 ];
          deck = newdeck;
          flop_turn_river = [];
          pot = 0;
          current_bet = 0;
          last_raise = Player.none_player;
          round_chips = (List.nth gm.players 0).chips;
          total_bet = 0;
        }
      else if p3 = plyr then
        {
          players =
            List.map
              (fun x -> x |> Player.unfold |> Player.reset_hand)
              [ p1; p2; Player.add_chips p3 gm.pot; p4; p5; p6 ];
          deck = newdeck;
          flop_turn_river = [];
          pot = 0;
          current_bet = 0;
          last_raise = Player.none_player;
          round_chips = (List.nth gm.players 0).chips;
          total_bet = 0;
        }
      else if p4 = plyr then
        {
          players =
            List.map
              (fun x -> x |> Player.unfold |> Player.reset_hand)
              [ p1; p2; p3; Player.add_chips p4 gm.pot; p5; p6 ];
          deck = newdeck;
          flop_turn_river = [];
          pot = 0;
          current_bet = 0;
          last_raise = Player.none_player;
          round_chips = (List.nth gm.players 0).chips;
          total_bet = 0;
        }
      else if p5 = plyr then
        {
          players =
            List.map
              (fun x -> x |> Player.unfold |> Player.reset_hand)
              [ p1; p2; p3; p4; Player.add_chips p5 gm.pot; p6 ];
          deck = newdeck;
          flop_turn_river = [];
          pot = 0;
          current_bet = 0;
          last_raise = Player.none_player;
          round_chips = (List.nth gm.players 0).chips;
          total_bet = 0;
        }
      else if p6 = plyr then
        {
          players =
            List.map
              (fun x -> x |> Player.unfold |> Player.reset_hand)
              [ p1; p2; p3; p4; p5; Player.add_chips p6 gm.pot ];
          deck = newdeck;
          flop_turn_river = [];
          pot = 0;
          current_bet = 0;
          last_raise = Player.none_player;
          round_chips = (List.nth gm.players 0).chips;
          total_bet = 0;
        }
      else failwith "won_round"
  | _ -> failwith "won_round"

let pick_round_winner gm =
  let still_in =
    List.filter
      (fun (x : Player.t) -> if x.folded = true then false else true)
      gm.players
  in
  if List.length still_in = 1 then (
    let w = List.nth still_in 0 in
    print_endline ("The player who wins is: " ^ Player.p_to_string w);
    won_round gm w)
  else
    let winners = List.sort Player.compare still_in in
    match winners with
    | w :: _ ->
        print_endline ("The player who wins is: " ^ Player.p_to_string w);
        won_round gm w
    | _ -> failwith "pick_winner"
