type document = block list
[@@deriving show { with_path = false }]

and block =
  | Header of int * inline list
  | Paragraph of inline list
  | List of int * inline list
  | NumList of int * inline list
  (*
    for future list parsing
    List of inline list
    NumList of inline list
  *)
  | ListItem of block list
  | NumListItem of block list
  | Table of table_block list list
  | Hrule
[@@deriving show { with_path = false }]

and inline =
  | NamedLink of inline list
  | Link of inline list
  | Bold of inline list
  | Italic of inline list
  | String of string
[@@deriving show { with_path = false }]

and table_block =
  | TableHead of inline list
  | TableItem of inline list
[@@deriving show { with_path = false }]

(* [@@deriving show] va créer automatique les fonctions
   [show_document], [show_block], ... *)
