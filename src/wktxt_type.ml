type document = block list
[@@deriving show { with_path = false }]

and block =
  | Header of int * inline list
  | Paragraph of inline list
  | List of int * inline list
  | NumList of int * inline list
  | Table of inline list
  | Hrule
[@@deriving show { with_path = false }]

(* à terme, List et Numlist comprendront des List_elem et NumList_elem
  ces derniers porteront les numéros d'importance à la place de List et NumList
*)
and inline =
  | NamedLink of inline list
  | Link of inline list
  | TableLine of inline list
  | List_elem of int * inline list
  | NumList_elem of int * inline list
  | Bold of inline list
  | Italic of inline list
  | Bolditalic of inline list
  | String of string
[@@deriving show { with_path = false }]

(* [@@deriving show] va créer automatique les fonctions
   [show_document], [show_block], ... *)
