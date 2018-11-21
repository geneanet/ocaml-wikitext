type document = block list
[@@deriving show { with_path = false }]

and block =
  | Header of int * inline list
  | Paragraph of inline list
  | List of int * inline list
  | NumList of int * inline list
  (*
    List of block list list
    NumList of block list list
  *)
  | Definition of def_line list
  | Table of table_block list list
  | Hrule
[@@deriving show { with_path = false }]

and inline =
  | Bold of inline list
  | Italic of inline list
  | String of string
  | Link of string
  | ExtLink of string
[@@deriving show { with_path = false }]

and table_block =
  | TableHead of inline list
  | TableItem of inline list

and def_line =
  | Term of inline list
  | Def of inline list
[@@deriving show { with_path = false }]

(* [@@deriving show] va créer automatique les fonctions
   [show_document], [show_block], ... *)
