open Poker

let start_game () =
  let _ = print_endline "You are playing Poker! (Texas hold 'em rules)" in
  let start = Game.newgame in
  let _ = print_endline "These are your cards to start the game:" in
  match start.players with
  | p1 :: _ -> print_endline (Hand.string_of_hand (snd p1))
  | _ -> ()
(* bet () *)

let _ = start_game ()
