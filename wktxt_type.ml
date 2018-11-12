type document = fragment list
[@@deriving show]

and fragment =
  | Header of int * fragment list
  | String of string
  | Char of char
  | White
[@@deriving show]
(* [@@deriving show] va créer automatique les fonctions
   [show_document], [show_fragment], ... *)
