open Poker

let () = Random.self_init ()

(** Records the user's bet from input *)
let rec get_user_bet min max =
  try
    let bt = int_of_string (read_line ()) in
  if bt > max then
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
      ^ string_of_int (gm.total_bet - (gm.round_chips - pl.chips)) 
      ^ " chips.")
  in
  let bet_size = get_user_bet (gm.total_bet - (gm.round_chips - pl.chips)) pl.chips in
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
  let rand = Random.int 10 in
  match (pl.player_type) with
  | Bot 1 -> begin (*A balanced player, a little insecure and annoying*)
    match rand with
  | 0 -> 
  print_endline "Bot 1 says: 'I call your bet (could be a check.)";
  print_endline "You can't hang with a bot like me!'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet;
  | 1 -> 
  print_endline "Bot 1 says: 'I call your bet (could be a check.)"; 
  print_endline "Guys, please be nice!'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
  | 2 -> 
  print_endline "Bot 1 says: 'I call your bet (could be a check.)"; 
  print_endline "I'm reading you like a book.'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
  | 3 -> 
  print_endline "Bot 1 says: 'I call your bet (could be a check.)"; 
  print_endline "Pleassssee let me hit my hand for once...'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
  | 4 -> 
  print_endline "Bot 1 says: 'I call your bet (could be a check.)"; 
  print_endline "My cards say call.'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
  | 5 -> 
  print_endline "Bot 1 says: 'I call your bet (could be a check.)"; 
  print_endline "You're bleeding me dry user!'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
  | 6 -> 
  print_endline "Bot 1 says: 'I call your bet (could be a check.)"; 
  print_endline "Do you guys even like me or do you want my money?'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
  | 7 -> 
  print_endline "Bot 1 says: 'I fold.'"; 
  print_endline "I'm not getting nothing tonight!'";
  print_endline "";
  Game.fold_player gm pl
  | 8 -> 
  print_endline "Bot 1 says: 'I fold.'"; 
  print_endline "I just can't right now.'";
  print_endline "";
  Game.fold_player gm pl
  | 9 ->
  print_endline "Bot 1 says: 'I'm all in!'"; 
  print_endline "It's do or die baby!'";
  print_endline "";
  Game.player_bet gm pl pl.chips
  | _ -> print_endline "This is not supposed to happen in bot 1 betting";
  gm
end
| Bot 2 -> begin (*Conservative player, doesn't like bot 1*)
  match rand with
| 0 -> 
print_endline "Bot 2 says: 'I fold.";
print_endline "Gotta keep it tight!'";
print_endline "";
Game.fold_player gm pl
| 1 -> 
  print_endline "Bot 2 says: 'I fold.";
  print_endline "Bro, bot 1, keep it together.'";
  print_endline "";
  Game.fold_player gm pl
| 2 -> 
  print_endline "Bot 2 says: 'I fold.";
  print_endline "Not wasting my dough.'";
  print_endline "";
  Game.fold_player gm pl
| 3 -> 
  print_endline "Bot 2 says: 'I fold.";
  print_endline "Gotta know when to fold 'em.'";
  print_endline "";
  Game.fold_player gm pl
| 4 -> 
  print_endline "Bot 2 says: 'I fold.";
  print_endline "Not this time.'";
  print_endline "";
  Game.fold_player gm pl
| 5 -> 
  print_endline "Bot 2 says: 'I fold.";
  print_endline "Bot 1, if you don't quit yapping...'";
    print_endline "";print_endline "";
  Game.fold_player gm pl
| 6 -> 
  print_endline "Bot 2 says: 'I fold.";
  print_endline "The cards come when they come.'";
  print_endline "";
  Game.fold_player gm pl
| 7 -> 
  print_endline "Bot 2 says: 'I fold.";
  print_endline "Not my round.'";
  print_endline "";
  Game.fold_player gm pl
| 8 -> 
  print_endline "Bot 2 says: 'I call your bet (could be a check.)"; 
    print_endline "I think I'll give these cards a shot'";
    print_endline "";
    Game.player_bet gm pl gm.current_bet
| 9 -> if gm.current_bet >= pl.chips - 20 then begin
print_endline "Bot 2 says: 'I call your bet (could be a check.)"; 
print_endline "I think I'll see this one out.'";
print_endline "";
Game.player_bet gm pl gm.current_bet 
end
else begin
  print_endline "Bot 2 says: 'I raise 20."; 
  print_endline "Could be bluffing...Gotta pay to see.'";
  print_endline "";
  Game.player_bet gm pl (gm.current_bet + 20)
end
| _ -> print_endline "This is not supposed to happen in bot 2 betting";
gm
end
| Bot 3 -> begin (*More agressive player, doesn't like bot 1*)
match rand with
| 0 -> 
  print_endline "Bot 3 says: 'I call your bet (could be a check.)"; 
    print_endline "Can't fold these cards.'";
    print_endline "";
    Game.player_bet gm pl gm.current_bet
| 1 -> 
  print_endline "Bot 3 says: 'I call your bet (could be a check.)"; 
    print_endline "You know bot 1 got held back twice in 1st grade?'";
    print_endline "";
    Game.player_bet gm pl gm.current_bet
| 2 -> 
  print_endline "Bot 3 says: 'I call your bet (could be a check.)"; 
  print_endline "Oh yes we call with this, yes we do.'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
| 3 -> 
  print_endline "Bot 3 says: 'I call your bet (could be a check.)"; 
  print_endline "Easy call.'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
| 4 -> 
  print_endline "Bot 3 says: 'I call your bet (could be a check.)"; 
  print_endline "I'm riding this one to the end.'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
| 5 -> 
  print_endline "Bot 3 says: 'I call your bet (could be a check.)"; 
  print_endline "Bot 1, when's the last time you won a pot, huh?.'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
| 6 -> 
  print_endline "Bot 3 says: 'I call your bet (could be a check.)"; 
    print_endline "I got this.'";
    print_endline "";
    Game.player_bet gm pl gm.current_bet
| 7 -> 
print_endline "Bot 3 says: 'I fold.";
print_endline "Alright, I can't play this hand.'";
print_endline "";
Game.fold_player gm pl
| 8 -> if gm.current_bet >= pl.chips - 30 then begin
  print_endline "Bot 3 says: 'I call your bet (could be a check.)"; 
  print_endline "I am the best poker player in the worlddd!!'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet 
  end
  else begin
  print_endline "Bot 3 says: 'I raise 30."; 
  print_endline "Time to crank up the heat...'";
  print_endline "";
  Game.player_bet gm pl (gm.current_bet + 30)
  end
| 9 -> print_endline "Bot 3 says: 'I'm all in!'"; 
print_endline "Alright, time to gun it.'";
print_endline "";
Game.player_bet gm pl pl.chips
| _ -> print_endline "This is not supposed to happen in bot 3 betting";
gm
end
| Bot 4 -> begin (*Calls and folds, never raises, susinct*)
match rand with
| 0 -> 
  print_endline "Bot 4 says: 'I call your bet (could be a check.)"; 
    print_endline "Yup call.'";
    print_endline "";
    print_endline "";
    Game.player_bet gm pl gm.current_bet
| 1 -> 
  print_endline "Bot 4 says: 'I call your bet (could be a check.)"; 
    print_endline "I'll call this thanks.'";
    print_endline "";
    Game.player_bet gm pl gm.current_bet
| 2 -> 
  print_endline "Bot 4 says: 'I call your bet (could be a check.)"; 
  print_endline "Call this every time.'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
| 3 -> 
  print_endline "Bot 4 says: 'I call your bet (could be a check.)"; 
  print_endline "Uh oh, kinda nervous about this.'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
| 4 -> 
  print_endline "Bot 4 says: 'I call your bet (could be a check.)"; 
  print_endline "Bot 1, please stop.'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
| 5 -> 
  print_endline "Bot 4 says: 'I call your bet (could be a check.)"; 
  print_endline "Bot 1, when's the last time you won a pot, huh?.'";
  print_endline "";
  Game.player_bet gm pl gm.current_bet
| 6 -> 
  print_endline "Bot 4 says: 'I fold.";
  print_endline "No thank you.'";
  print_endline "";
  Game.fold_player gm pl
| 7 -> 
print_endline "Bot 4 says: 'I fold.";
print_endline "Goodbye.'";
print_endline "";
Game.fold_player gm pl
| 8 -> print_endline "Bot 4 says: 'I fold.";
print_endline "Nope.'";
print_endline "";
Game.fold_player gm pl
| 9 -> print_endline "Bot 4 says: 'I fold.";
print_endline "Bot 1, please leave.'";
print_endline "";
Game.fold_player gm pl
| _ -> print_endline "This is not supposed to happen in bot 4 betting";
gm
end
  | Bot 5 -> begin (*Folds and raises, never calls.*)
  match rand with
  | 0 -> 
    print_endline "Bot 5 says: 'I fold.";
    print_endline "Not this one.'";
    print_endline "";
    Game.fold_player gm pl
  | 1 -> 
    print_endline "Bot 5 says: 'I fold.";
    print_endline "Bot 1, that's enough now...'";
    print_endline "";
    Game.fold_player gm pl
  | 2 -> 
    print_endline "Bot 5 says: 'I fold.";
    print_endline "Eh, I'll lay it down.'";
    print_endline "";
    Game.fold_player gm pl
  | 3 -> 
    print_endline "Bot 5 says: 'I fold.";
    print_endline "Dude, these cards are not it.'";
    print_endline "";
    Game.fold_player gm pl
  | 4 -> 
    print_endline "Bot 5 says: 'I fold.";
    print_endline "Dude, 7-2 off-suit..are you serious?'";
    print_endline "";
    Game.fold_player gm pl
  | 5 -> 
    print_endline "Bot 5 says: 'I call your bet (if you bet it bet the same, or checks if you checked.)"; 
    print_endline "Bot 1 has serious issuess...'";
    print_endline "";
    Game.player_bet gm pl gm.current_bet
  | 6 -> 
    print_endline "Bot 5 says: 'I call your bet (if you bet it bet the same, or checks if you checked.)"; 
    print_endline "Hmmmmmmm......ths one is tight.'";
    print_endline "";
    Game.player_bet gm pl gm.current_bet
  | 7 -> if gm.current_bet >= pl.chips - 40 then begin
    print_endline "Bot 5 says: 'I call your bet (if you bet it bet the same, or checks if you checked.)"; 
    print_endline "I don't bluff. Just win.'";
    print_endline "";
    Game.player_bet gm pl gm.current_bet 
    end
    else begin
    print_endline "Bot 5 says: 'I raise 40."; 
    print_endline "Time to lay down the hammer...'";
    print_endline "";
    Game.player_bet gm pl (gm.current_bet + 40)
    end
  | 8 -> if gm.current_bet >= pl.chips - 10 then begin
    print_endline "Bot 5 says: 'I call your bet (if you bet it bet the same, or checks if you checked.)"; 
    print_endline "Show me the moneyyy!'";
    print_endline "";
    Game.player_bet gm pl gm.current_bet 
    end
    else begin
    print_endline "Bot 5 says: 'I raise 10."; 
    print_endline "Calm 10 raise...'";
    print_endline "";
    Game.player_bet gm pl (gm.current_bet + 10)
    end
  | 9 ->if gm.current_bet >= pl.chips - 25 then begin
    print_endline "Bot 5 says: 'I call your bet (if you bet it bet the same, or checks if you checked.)"; 
    print_endline "Stop playing bad!'";
    print_endline "";
    Game.player_bet gm pl gm.current_bet 
    end
    else begin
    print_endline "Bot 5 says: 'I raise 25."; 
    print_endline "Bot 5 doesn't mess around!'";
    print_endline "";
    Game.player_bet gm pl (gm.current_bet + 25)
    end
  | _ -> print_endline "This is not supposed to happen in bot 5 betting";
  gm
  end
  | _ -> gm

(** [bet g] is the game [g] after one further round of betting to completion, filtering made with help of GPT 5/15 *)
let bet (gm : Game.t) =
  let playerlist = gm.players in
  let rec betfun (g : Game.t) ilst =
    let active_players = List.filter (fun p -> not p.Player.folded) gm.players in
    if List.length active_players = 1 then begin
      print_endline (Player.p_to_string (List.hd active_players) ^ " wins a pot of " ^ string_of_int g.pot ^ "!!");
      exit 1
    end else 
    match ilst with
    | p :: t ->
        if Player.p_to_string g.last_raise = Player.p_to_string p then
          { g with last_raise = Player.none_player }  (* Resetting last_raise *)
        else if p.folded then
          betfun g t  (* Skip folded players *)
        else if p.player_type = User then
          let gme = user_action g p in
          betfun gme t
        else
          let gme = bot_bet g p in
          betfun gme t
    | [] -> if g.last_raise = Player.none_player then g else betfun g g.players  (* Restart betting if needed *)
  in
  betfun gm playerlist

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
  let first_round = bet start in
  
  Game.print_game (first_round);
  
  let flop_deck = Game.deal_flop (first_round) in

  let second_round = bet flop_deck in 

  Game.print_game (second_round);

  let turn = Game.deal_turn (second_round) in
  
  let third_round = bet turn in

  Game.print_game (third_round);

  let river = Game.deal_river (turn) in

  let fourth_round = bet river in

  Game.print_game (fourth_round)


   (*only print players left in hand*)
(* let flop () = *)

(* bet () *)
let _ = start_game ()
