open OUnit2
open Wktxt_type

let assert_equal expected input =
  let lexbuf = Lexing.from_string input in
  let ast = Wktxt_parser.document Wktxt_lexer.main lexbuf in
  assert_equal ~printer:Wktxt_type.show_document expected ast

let bold_test_1 _ctx =
  assert_equal
    [ Paragraph [ String "Some "
                ; Bold [ String "bold text" ]
                ; String " in the middle of the line"
                ]
    ]
    "Some '''bold text''' in the middle of the line"

let bold_test_2 _ctx =
  assert_equal
    [ Paragraph [ Bold [ String "bold text" ]
                ; String " at begining of a line"
                ]
    ]
    "'''bold text''' at begining of a line"

let bold_test_3 _ctx =
  assert_equal
    [ Paragraph [ String "bold text at "
                ; Bold [ String "end of line" ]
                ]
    ]
    "bold text at '''end of line'''"

let () =
  run_test_tt_main
    ("wktxt" >::: [ "bold_test_1" >:: bold_test_1
                  ; "bold_test_2" >:: bold_test_2
                  ; "bold_test_3" >:: bold_test_3
                  ])
