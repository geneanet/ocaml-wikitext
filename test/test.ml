open OUnit2
open Wktxt_type

let assert_equal expected input =
  Wktxt_lexer.newline := true ;
  Wktxt_lexer.last_def_term_line := 0 ;
  Wktxt_lexer.last_def_term_depth := 0 ;
  let lexbuf = Lexing.from_string input in
  let ast = Wktxt_parser.document Wktxt_lexer.main lexbuf in
  assert_equal ~printer:Wktxt_type.show_document expected ast

let bold_1 _ctx =
  assert_equal
    [ Paragraph [ String "Lorem "
                ; Bold [ String "ipsum dolor" ]
                ; String " sit amet"
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
                ; Italic [ String "end of line" ]
                ]
    ]
    "italic text at ''end of line''"

let italic_4 _ctx =
  assert_equal
    [ Paragraph [Italic [ String "italic text on a complete line" ] ] ]
    "''italic text on a complete line''"

let bolditalic_1 _ctx =
  assert_equal
    [ Paragraph [ String "Lorem "
                ; Bold [ Italic [ String "ipsum" ] ]
                ; String "."
                ]
    ]
    "Lorem '''''ipsum'''''."

let paragraph_1 _ctx =
  assert_equal
    [ Paragraph [ String "Lorem ipsum" ; String "\n"] ;
      Paragraph [ String "dolores sit amet"]
    ]
    "Lorem ipsum\n  \ndolores sit amet"

let space_1 _ctx =
  assert_equal
    [ Paragraph [ String "italic text at"
                ; Italic [ String " end of line" ]
                ]
    ]
    "italic text at'' end of line''"

let space_2 _ctx =
  assert_equal
    [ Paragraph [ String "Lorem ipsum" ; String "\n";
                  String "dolores"
                ]
    ]
    "Lorem ipsum\ndolores"

let link_1 _ctx =
  assert_equal
    [ Paragraph [ String "This is an "; ExtLink "www.test.com external link"
                ; String "."
                ]
    ]
    "This is an [www.test.com external link]."

let link_2 _ctx =
  assert_equal
    [ Paragraph [ String "This is a "; Link "PageName|link"
                ; String "."
                ]
    ]
    "This is a [[PageName|link]]."

let list_1 _ctx =
  assert_equal
    [ List [ [ Paragraph [ String "1" ; String "\n" ] ]
           ; [ Paragraph [ String "2" ; String "\n" ] ]
           ; [ Paragraph [ String "3" ] ]
        ]
    ]
    "* 1\n\
     * 2\n\
     * 3"

let list_2 _ctx =
  assert_equal
    [ List [ [ Paragraph [ String "1" ; String "\n" ]
             ; List [ [ Paragraph [ String "1.1" ; String "\n" ] ]
                    ; [ Paragraph [ String "1.2" ; String "\n" ]
                      ; List [ [ Paragraph [ String "1.2.1" ; String "\n" ] ] ]
                      ]
                 ]
             ]
           ; [ Paragraph [ String "2" ] ]
        ]
    ]
    "* 1\n\
     ** 1.1\n\
     ** 1.2\n\
     *** 1.2.1\n\
     * 2"

let list_3 _ctx =
  assert_equal
    [ List [ [ Paragraph [ String "1" ]]]
    ]
    "* 1"

let list_4 _ctx =
  assert_equal
    [ List [[ List [ [ Paragraph [ String "1" ]]]]]
    ]
    "** 1"

let list_5 _ctx =
  assert_equal
    [ NumList [[ Paragraph [ String "1"]]]]
    "# 1"

let list_6 _ctx =
  assert_equal
    [ NumList [[ NumList [ [ Paragraph [ String "1" ]]]]]
    ]
    "## 1"

let list_7 _ctx =
  assert_equal
    [ List [ [ Paragraph [ String "1" ; String "\n" ]
             ; List [ [ List [ [ Paragraph [ String "1.1.1" ] ] ] ] ] ] ] ]
    "* 1\n\
     *** 1.1.1"

let list_8 _ctx =
  assert_equal
    [ List [ [ Paragraph [ String "1" ; String "\n" ]
             ; List [ [ List [ [ Paragraph [ String "1.1.1" ; String "\n"] ] ] ]
             ; [ Paragraph [ String "1.2" ] ] ] ]
           ]
    ]
    "* 1\n\
     *** 1.1.1\n\
     ** 1.2"
  
let list_mixed_1 _ctx =
  assert_equal
    [ NumList [[ Paragraph [ String "1" ; String "\n" ] 
                ; List [[ Paragraph [String "1.1" ; String "\n"] ]] ]
              ; [ Paragraph [ String "2" ] ]
              ]
    ]
    "# 1\n\
     **1.1\n\
     # 2"

let list_mixed_2 _ctx =
  assert_equal
    [ List [[ Paragraph [ String "1" ; String "\n" ] 
                ; NumList [[ Paragraph [String "1.1" ; String "\n"] ]] ]
              ; [ Paragraph [ String "2" ] ]
              ]
    ]
    "* 1\n\
     ##1.1\n\
     * 2"

let list_mixed_3 _ctx = (* no warning should be written on STDERR *)
  assert_equal
    [ 
        List [[ Paragraph [ String "1" ; String "\n" ]]]
      ; NumList [[ Paragraph [ String "2" ; String "\n" ]]]
      ; List [[ Paragraph [ String "3" ]]]
    ]
    "* 1\n\
     # 2\n\
     * 3"

let list_mixed_4 _ctx = (* no warning should be written on STDERR *)
  assert_equal
    [ 
        NumList [[ Paragraph [ String "1" ; String "\n" ]]]
      ; List [[ Paragraph [ String "2" ; String "\n" ]]]
      ; NumList [[ Paragraph [ String "3" ]]]
    ]
    "# 1\n\
     * 2\n\
     # 3"

let list_mixed_5 _ctx = (* no warning should be written on STDERR *)
  assert_equal
    [ 
        List [[ Paragraph [ String "1" ; String "\n"] ; 
              NumList [[ Paragraph [ String "1.1" ; String "\n" ]]]
             ]]
      ; NumList [[ Paragraph [ String "3" ]]]
    ]
    "* 1\n\
     ## 1.1\n\
     # 3"

let list_mixed_warning_1 _ctx = (* a warning should be written on STDERR for this test *)
  assert_equal
    [
        List  [[ Paragraph [String "1" ; String "\n"]
                ; NumList [ [ Paragraph [ String "1.1" ; String "\n" ]]
                            ; [ Paragraph [ String "1.2" ]] ]
              ]]
    ]
    "* 1\n\
     ## 1.1\n\
     ** 1.2"

let def_1 _ctx =
  assert_equal
    [ DefList [ ([String "t1"], [
                  Paragraph [ String "d1" ]
                  ])
              ] ]
    ";t1:d1"

let def_2 _ctx =
  assert_equal
    [ DefList [ ([String "t1" ; String "\n"], [
                    Paragraph [ String "d1" ]
                  ])
              ] ]
    ";t1\n\
     :d1"

let def_3 _ctx =
  assert_equal
    [ DefList [ ([String "t1" ; String "\n"],
    [ Paragraph [ String "d1" ; String "\n" ]
                  ; Paragraph [ String "d2" ]
                  ])
              ] ]
    ";t1\n\
     :d1\n\
     :d2"

let def_4 _ctx =
  assert_equal
    [ DefList [ ([], [ Paragraph [ String "d1" ] ])
              ] ]
    ":d1"

let def_5 _ctx =
  assert_equal
    [ DefList [ ([String "t1"]), []]
    ]
    ";t1"

let def_6 _ctx =
  assert_equal
    [ DefList [ ([String "t1"], [ Paragraph [ String "d1" ; String "\n" ]])
                ; ([String "t2"], [ Paragraph [ String "d2" ]])]
    ]
    ";t1:d1\n\
     ;t2:d2"

let def_7 _ctx =
  assert_equal
    [ DefList [ ([String "t1"], [ Paragraph [ String "d1" ; String "\n" ]
                                  ; DefList [ ([String "t1.1"], [ Paragraph [ String "d1.1" 
                                                                            ; String "\n" ]])]
                                  ])
                ; ([String "t2"], [ Paragraph [ String "d2" ] ])
              ]
    ]
    ";t1:d1\n\
     :;t1.1:d1.1\n\
     ;t2:d2"

let def_8 _ctx =
  assert_equal
    [DefList [ ([] , [DefList [ ([String "t1"], [Paragraph [ String "d1" ]])]])]]
    ":;t1:d1"

let def_9 _ctx =
  assert_equal
    [ DefList [ ([String "t1"], [ Paragraph [ String "d1" ; String "\n" ]
                                  ; DefList [ ([String "t1.1"], [ Paragraph [ String "d"
                                                                            ; String ":::"
                                                                            ; String "1.1" 
                                                                            ; String "\n" ]])]
                                  ])
                ; ([String "t2"], [ Paragraph [ String "d2" ] ])
              ]
    ]
    ";t1:d1\n\
     :;t1.1:d:::1.1\n\
     ;t2:d2"

let table_1 _ctx =
  assert_equal
    [ Table ([String "Titre" ; String "\n"], [  [ TableItem []
                                                ; TableHead [String "Titre A" ; String "\n"]
                                                ; TableHead [String "Titre B" ; String "\n"]
                                                ; TableHead [String "Titre C" ; String "\n"]
                                                ]
                                              ; [ TableHead [String "Titre 1" ; String "\n"]
                                                ; TableItem [String "Data 1A" ; String "\n"]
                                                ; TableItem [String "Data 1B" ; String "\n"]
                                                ; TableItem [String "Data 1C" ; String "\n"]
                                                ]
                                              ; [ TableHead [String "Titre 2" ; String "\n"]
                                                ; TableItem [String "Data 1A" ; String "\n"]
                                                ; TableItem [String "Data 1B" ; String "\n"]
                                                ; TableItem [String "Data 1C" ; String "\n"]
                                                ]
                                             ])
    ]
    "{|\n\
      |-\n\
      |\n\
      ! Titre A\n\
      ! Titre B\n\
      ! Titre C\n\
      |-\n\
      ! Titre 1\n\
      | Data 1A\n\
      | Data 1B\n\
      | Data 1C\n\
      |-\n\
      ! Titre 2\n\
      | Data 2A\n\
      | Data 2B\n\
      | Data 2C\n\
      |}"

let table_2 _ctx =
  assert_equal
    [ Table ([String "Titre" ; String "\n"], [  [ TableItem []
                                                ; TableHead [String "Titre A" ; String "\n"]
                                                ; TableHead [String "Titre B" ; String "\n"]
                                                ; TableHead [String "Titre C" ; String "\n"]
                                                ]
                                              ; [ TableHead [String "Titre 1" ; String "\n"]
                                                ; TableItem [String "Data 1A" ; String "\n"]
                                                ; TableItem [String "Data 1B" ; String "\n"]
                                                ; TableItem [String "Data 1C" ; String "\n"]
                                                ]
                                              ; [ TableHead [String "Titre 2" ; String "\n"]
                                                ; TableItem [String "Data 1A" ; String "\n"]
                                                ; TableItem [String "Data 1B" ; String "\n"]
                                                ; TableItem [String "Data 1C" ; String "\n"]
                                                ]
                                             ])
    ]
    "{|\n\
      |+ Titre\n\
      |----\n\
      ! !! Titre col. A !! Titre col. B !! Titre col. C\n\
      |----\n\
      ! Titre ligne 1\n\
      | Data 1A || Data 1B || Data 1C\n\
      |----\n\
      ! Titre ligne 2\n\
      | Data 2A || Data 2B || Data 2C\n\
      |}"

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
                  ; "paragraph_1" >:: paragraph_1
                  ; "space_1" >:: space_1
                  ; "space_2" >:: space_2
                  ; "link_1" >:: link_1
                  ; "link_2" >:: link_2
                  ; "list_3" >:: list_3
                  ; "list_4" >:: list_4
                  ; "list_5" >:: list_5
                  ; "list_6" >:: list_6
                  ; "list_7" >:: list_7
                  ; "list_8" >:: list_8
                  ; "list_mixed_1" >:: list_mixed_1
                  ; "list_mixed_2" >:: list_mixed_2
                  ; "list_mixed_3" >:: list_mixed_3
                  ; "list_mixed_4" >:: list_mixed_4
                  ; "list_mixed_5" >:: list_mixed_5
                  ; "list_mixed_warning_1" >:: list_mixed_warning_1
                  ; "def_1" >:: def_1
                  ; "def_2" >:: def_2
                  ; "def_3" >:: def_3
                  ; "def_4" >:: def_4
                  ; "def_5" >:: def_5
                  ; "def_6" >:: def_6
                  ; "def_7" >:: def_7
                  ; "def_8" >:: def_8
                  ; "def_9" >:: def_9
                  (*
                  ; "table_1" >:: table_1
                  ; "table_2" >:: table_2
                  *)
                  ])
