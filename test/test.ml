open OUnit2
open Wktxt_type

let assert_equal expected input =
  let lexbuf = Lexing.from_string input in
  let ast = Wktxt_parser.document Wktxt_lexer.main lexbuf in
  assert_equal ~printer:Wktxt_type.show_document expected ast

let bold_1 _ctx =
  assert_equal
    [ Paragraph [ String "Lorem "
                ; Bold [ String "ipsum dolor" ]
                ; String "  sit amet"
                ]
    ]
    "Lorem '''ipsum dolor''' sit amet"

let bold_2 _ctx =
  assert_equal
    [ Paragraph [ Bold [ String "bold text" ]
                ; String " at begining of a line"
                ]
    ]
    "'''bold text''' at begining of a line"

let bold_3 _ctx =
  assert_equal
    [ Paragraph [ String "bold text at "
                ; Bold [ String "end of line" ]
                ]
    ]
    "bold text at '''end of line'''"


let italic_1 _ctx =
  assert_equal
    [ Paragraph [ Italic [ String "italic text" ]
                ; String " at begining of a line"
        ]
    ]
    "''italic text'' at begining of a line"

let italic_2 _ctx =
  assert_equal
    [ Paragraph [ String "Some "
                ; Italic [ String "italicize text" ]
                ; String " in the middle of the line"
                ]
    ]
    "Some ''italicize text'' in the middle of the line"

let italic_3 _ctx =
  assert_equal
    [ Paragraph [ String "italic text at "
                ; Bold [ String "end of line" ]
                ]
    ]
    "italic text at ''end of line''"

let italic_4 _ctx =
  assert_equal
    [ Paragraph [ String "italic text on a complete line" ] ]
    "''italic text on a complete line''"

let bolditalic_1 _ctx =
  assert_equal
    [ Paragraph [ String "Lorem "
                ; Bold [ Italic [ String "ipsum" ] ]
                ; String "."
                ]
    ]
    "Lorem '''''ipsum'''''."


let () =
  run_test_tt_main
    ("wktxt" >::: [ "bold_1" >:: bold_1
                  ; "bold_2" >:: bold_2
                  ; "bold_3" >:: bold_3
                  ; "italic_1" >:: italic_1
                  ; "italic_2" >:: italic_2
                  ; "italic_3" >:: italic_3
                  ; "italic_4" >:: italic_4
                  ; "bolditalic_1" >:: bolditalic_1
                  ])
