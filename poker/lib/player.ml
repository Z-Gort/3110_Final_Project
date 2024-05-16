type ptype =
  | User
  | Bot of int
  | None

type t = {
  player_type : ptype;
  hand : Hand.t;
  chips : int;
  folded : bool;
}

let default_chips = 1000

let new_user =
  {
    player_type = User;
    hand = Hand.empty;
    chips = default_chips;
    folded = false;
  }

let none_player =
  { player_type = None; hand = Hand.empty; chips = 0; folded = false }

let new_bot i =
  {
    player_type = Bot i;
    hand = Hand.empty;
    chips = Int.max_int - 9000;
    folded = false;
  }

(* exception InsufficientFunds *)

let subtract_chips p n =
  match p with
  | { player_type = pt; hand = h; chips = c; folded = f } ->
      { player_type = pt; hand = h; chips = c - n; folded = f }

let add_chips p n =
  match p with
  | { player_type = pt; hand = h; chips = c; folded = f } ->
      { player_type = pt; hand = h; chips = c + n; folded = f }

let deal_card p crd =
  match p with
  | { player_type = pt; hand = h; chips = c; folded = f } ->
      { player_type = pt; hand = Hand.add crd h; chips = c; folded = f }

let fold pl = { pl with folded = true }
let unfold pl = { pl with folded = false }
let reset_hand pl = { pl with hand = [] }

let print_player pl =
  match pl with
  | { player_type = Bot i; hand = hnd; chips = chps; folded = fld } ->
      print_endline ("Player type: Bot " ^ string_of_int i);
      print_endline ("Hand: " ^ Hand.string_of_hand hnd);
      print_endline ("Chips: " ^ string_of_int chps);
      print_endline ("Folded: " ^ string_of_bool fld)
  | { player_type = User; hand = hnd; chips = chps; folded = fld } ->
      print_endline "Player type: User";
      print_endline ("Hand: " ^ Hand.string_of_hand hnd);
      print_endline ("Chips: " ^ string_of_int chps);
      print_endline ("Folded: " ^ string_of_bool fld)
  | _ -> print_endline "none_player"

let p_to_string p =
  match p.player_type with
  | User -> "user"
  | Bot k -> "bot " ^ string_of_int k
  | None -> "none_player"

let compare p1 p2 =
  match (p1, p2) with
  | ( { player_type = _; hand = h1; chips = _; folded = _ },
      { player_type = _; hand = h2; chips = _; folded = _ } ) ->
      Hand.compare h1 h2
