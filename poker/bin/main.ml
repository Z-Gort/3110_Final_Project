open Poker

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

let rec user_action (gm : Game.t) (pl : Player.t) =
  let _ = print_endline "What would you like to do? [ Check | Bet | Fold ]" in
  let input = read_line () in
  match input with
  | "Check" | "check" -> gm
  | "Bet" | "bet" -> user_bet gm pl
  | "Fold" | "fold" -> gm (* change this *)
  | _ ->
      let _ =
        print_endline "Please enter only \"Check\", \"Bet\", or \"Fold\""
      in
      user_action gm pl

let rec bet (gm : Game.t) =
  match gm.players with
  | p :: _ (* t? instead of _ *) ->
      if p.player_type = User then
        let _ = user_action gm p in
        ()
  | _ -> ()

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
