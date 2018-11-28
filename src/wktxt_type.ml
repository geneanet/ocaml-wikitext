type order = Ordered | Unordered

type document = block list
[@@deriving show { with_path = false }]

and block =
  | Header of int * inline list
  | Paragraph of inline list
  | List of block list list
  | NumList of block list list
  | DefList of def_block list
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
[@@deriving show { with_path = false }]

and def_block = inline list * block list
[@@deriving show { with_path = false }]

(* [@@deriving show] va créer automatique les fonctions
   [show_document], [show_block], ... *)
