type ptype =
  | User
  | Bot of int

type t = {
  player_number : int;
  player_type : ptype;
  hand : Hand.t;
  chips : int;
}

exception InsufficientFunds

let subtract_chips p n =
  match p with
  | { player_number = pn; player_type = pt; hand = h; chips = c } ->
      if c - n < 0 then raise InsufficientFunds
      else { player_number = pn; player_type = pt; hand = h; chips = c - n }

let add_chips p n =
  match p with
  | { player_number = pn; player_type = pt; hand = h; chips = c } ->
      { player_number = pn; player_type = pt; hand = h; chips = c + n }

let deal_card p crd =
  match p with
  | { player_number = pn; player_type = pt; hand = h; chips = c } ->
      { player_number = pn; player_type = pt; hand = Hand.add crd h; chips = c }
