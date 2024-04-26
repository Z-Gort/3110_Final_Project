type ptype =
  | User
  | Bot of int

type t =
  { player_type : ptype
  ; hand : Hand.t
  ; chips : int
  }

let default_chips = 100
let new_user = { player_type = User; hand = Hand.empty; chips = default_chips }
let new_bot i = { player_type = Bot i; hand = Hand.empty; chips = default_chips }

(* exception InsufficientFunds *)

let subtract_chips p n =
  match p with
  | { player_type = pt; hand = h; chips = c } ->
    { player_type = pt; hand = h; chips = c - n }
;;

let add_chips p n =
  match p with
  | { player_type = pt; hand = h; chips = c } ->
    { player_type = pt; hand = h; chips = c + n }
;;

let deal_card p crd =
  match p with
  | { player_type = pt; hand = h; chips = c } ->
    { player_type = pt; hand = Hand.add crd h; chips = c }
;;
