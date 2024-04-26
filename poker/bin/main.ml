open Poker

(** Prompts the user to bet *)
let rec user_bet (gm : Game.t) (pl : Player.t) =
  let _ =
    print_endline
      ("How much would you like to bet? You currently have "
     ^ string_of_int pl.chips ^ " chips and you must bet at least "
      ^ string_of_int gm.current_bet
      ^ " chips.")
  in
  let bet_size = int_of_string (read_line ()) in
  if bet_size > pl.chips then
    let _ = print_endline "You can only bet as many chips as you have!" in
    user_bet gm pl
  else if bet_size < gm.current_bet then
    let _ =
      print_endline
        ("You must bet at least " ^ string_of_int gm.current_bet ^ " chips!")
    in
    user_bet gm pl
  else Game.player_bet gm pl bet_size

(** Prompts the user to take an action in the betting round when it is their
    turn*)
let rec user_action (gm : Game.t) (pl : Player.t) =
  if pl.folded then gm
  else
    let _ = print_endline "What would you like to do? [ Check | Bet | Fold ]" in
    let input = read_line () in
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
    | "Bet" | "bet" -> user_bet gm pl
    | "Fold" | "fold" -> Game.fold_player gm pl
    | _ ->
        let _ =
          print_endline "Please enter only \"Check\", \"Bet\", or \"Fold\""
        in
        user_action gm pl

let bot_bet (gm : Game.t) (pl : Player.t) =
  match (gm, pl) with
  | _ -> gm (* unimplemented *)

(** [bet g] is the game [g] after one further round of betting to completion *)
let bet (gm : Game.t) =
  let playerlist = gm.players in
  let rec betfun (g : Game.t) ilst plst =
    match ilst with
    | p :: t ->
        if p = g.last_raise then { g with last_raise = Player.none_player }
        else if p.player_type = User then
          let gme = user_action g p in
          betfun gme t plst
        else
          let gme = bot_bet g p in
          betfun gme t plst
    | [] -> if g.last_raise = Player.none_player then g else betfun g plst plst
  in
  betfun gm playerlist playerlist

(** starts a game of poker *)
let start_game () =
  let _ = print_endline "You are playing Poker! (Texas hold 'em rules)" in
  let start = Game.newgame in
  let _ = print_endline "These are your cards to start the game:" in
  let _ =
    match start.players with
    | p1 :: _ -> print_endline (Hand.string_of_hand p1.hand)
    | _ -> ()
  in
  bet start
(* let flop () = *)

(* bet () *)
let _ = start_game ()
