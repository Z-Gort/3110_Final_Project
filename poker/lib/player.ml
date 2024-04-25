type ptype =
  | User
  | Bot of int

type t = {
  player_type : ptype;
  hand : Hand.t;
  chips : int;
  in_front : int;
}

let default_chips = 100
let default_in_front = 0
let new_user = { player_type = User; hand = Hand.empty; chips = default_chips; in_front = default_in_front}

let new_bot i =
  { player_type = Bot i; hand = Hand.empty; chips = default_chips; in_front = default_in_front}

exception InsufficientFunds

let subtract_chips p n =
  match p with
  | { player_type = pt; hand = h; chips = c; in_front = front} ->
 { player_type = pt; hand = h; chips = c - n; in_front = front + n }

let add_chips p n =
  match p with
  | { player_type = pt; hand = h; chips = c; in_front = front } ->
      { player_type = pt; hand = h; chips = c + n; in_front = front }

let deal_card p crd =
  match p with
  | { player_type = pt; hand = h; chips = c; in_front = front} ->
      { player_type = pt; hand = Hand.add crd h; chips = c; in_front = front }
