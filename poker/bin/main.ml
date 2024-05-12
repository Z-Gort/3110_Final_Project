open Poker

(** Records the user's bet from input *)
let rec get_user_bet min max =
  try
    let bt = int_of_string (read_line ()) in
    if bt = 0 then
      let _ = print_endline "You cannot bet zero chips! Enter a new bet: " in
      get_user_bet min max
    else if bt > max then
      let _ =
        print_endline
          "You can only bet as many chips as you have! Enter a new bet: "
      in
      get_user_bet min max
    else if bt < min then
      let _ =
        print_endline
          ("You must bet at least " ^ string_of_int min
         ^ " chips! Enter a new bet: ")
      in
      get_user_bet min max
    else bt
  with exn -> (
    match exn with
    | Failure _ ->
        let _ = print_endline "Please enter only a number! Enter a new bet: " in
        get_user_bet min max
    | exc -> raise exc)

(** Prompts the user to bet *)
let user_bet (gm : Game.t) (pl : Player.t) : Game.t =
  let _ =
    print_endline
      ("How much would you like to bet? You currently have "
     ^ string_of_int pl.chips ^ " chips and you must bet at least "
      ^ string_of_int gm.current_bet
      ^ " chips.")
  in
  let bet_size = get_user_bet gm.current_bet pl.chips in
  let newgm = Game.player_bet gm pl bet_size in
  newgm

(** Prompts the user to take an action in the betting round when it is their
    turn*)
let rec user_action (gm : Game.t) (pl : Player.t) =
  (* let _ = Player.print_player pl in *)
  if pl.folded then gm
  else
    let _ = print_endline "What would you like to do? < Check | Bet | Fold >" in
    let input = BatString.trim (read_line ()) in
    match input with
    | "Check" | "check" ->
        if gm.current_bet > 0 then
          let _ =
            print_endline
              ("You must bet at least "
              ^ string_of_int gm.current_bet
              ^ " chips!")
          in
          user_action gm pl
        else gm
    | "Bet" | "bet" ->
        let newgm = user_bet gm pl in
        newgm
    | "Fold" | "fold" ->
        let newgm = Game.fold_player gm pl in
        (* let _ = List.map Player.print_player gmeee.players in *)
        newgm
    | _ ->
        let _ =
          print_endline "Please enter only \"Check\", \"Bet\", or \"Fold\""
        in
        user_action gm pl

let bot_bet (gm : Game.t) (pl : Player.t) =
  match (gm, pl) with
  | _ -> gm
(* unimplemented *)

(** [bet g] is the game [g] after one further round of betting to completion *)
let bet (gm : Game.t) =
  let playerlist = gm.players in
  let rec betfun (g : Game.t) ilst plst =
    match ilst with
    | p :: t ->
        if p = g.last_raise then
          let endrnd = { g with last_raise = Player.none_player } in
          endrnd
        else if p.player_type = User then
          let gme = user_action g p in
          (* let _ = print_endline ("user's new money2: " ^ string_of_int (match
             gme.players with | h :: _ -> h.chips | _ -> 1)) in let _ =
             print_endline "last_raise: " in let _ = Player.print_player
             gme.last_raise in *)
          betfun gme t plst
        else
          let gme = bot_bet g p in
          betfun gme t plst
    | [] -> if g.last_raise = Player.none_player then g else betfun g plst plst
  in
  betfun gm playerlist playerlist

(** starts a game of poker *)
let start_game () =
  let _ = print_endline "You are playing Poker!\n" in
  let start = Game.newgame in
  let _ = print_endline "These are your cards to start the game:" in
  let _ =
    match start.players with
    | p1 :: _ -> print_endline (Hand.string_of_hand p1.hand)
    | _ -> ()
  in
  let _ = print_newline () in
  bet start
(* let flop () = *)

(* bet () *)
let _ = start_game ()

(* There is a bug, never stops betting, I dont know why :( ... )*)
