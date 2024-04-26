open Poker

let bet () = ()

let start_game () =
  let _ = print_endline "You are playing Poker! (Texas hold 'em rules)" in
  let start = Game.newgame in
  let _ = print_endline "These are your cards to start the game:" in
  let _ =
    match start.players with
    | p1 :: _ -> print_endline (Hand.string_of_hand p1.hand)
    | _ -> ()
  in
  bet ()
(* let flop () = *)

(* bet () *)
