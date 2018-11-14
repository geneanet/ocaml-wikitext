type document = block list
[@@deriving show]

and block =
  | Header of int * inline list
[@@deriving show]

and inline =
  | Bold of inline list
  | Italic of inline list
  | White
  | String of string
  | Char of char
[@@deriving show]
    
(* [@@deriving show] va créer automatique les fonctions
   [show_document], [show_block], ... *)
