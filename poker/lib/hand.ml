type t = Card.t list

let compare h1 h2 =
  match (h1, h2) with
  | _ -> 0
(* currently treats all hands as equal, needs to be fully implemented *)
