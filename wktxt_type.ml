type document = block list
[@@deriving show]

and block =
  | Header of int * inline list
  | Paragraph of inline list
  | List of int * inline list
  | Num_list of int * inline list
  | Hrule
[@@deriving show]

and inline =
  | Bold of inline list
  | Italic of inline list
  | String of string
[@@deriving show]
    
(* [@@deriving show] va créer automatique les fonctions
   [show_document], [show_block], ... *)
