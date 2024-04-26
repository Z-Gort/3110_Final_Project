type ptype =
  | User
  | Bot of int

type t = {
  player_type : ptype;
  hand : Hand.t;
  chips : int;
  folded : bool;
}

let default_chips = 100

let new_user =
  {
    player_type = User;
    hand = Hand.empty;
    chips = default_chips;
    folded = false;
  }

let none_player =
  { player_type = User; hand = Hand.empty; chips = Int.min_int; folded = false }

let new_bot i =
  {
    player_type = Bot i;
    hand = Hand.empty;
    chips = default_chips;
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
