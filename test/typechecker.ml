open Core
open Mini.Parser
open Mini.Typechecker

let hanoi = parse_file "mini/milestone2/benchmarks/hanoi_benchmark/hanoi_benchmark.mini"

let test_tc t s =
  (* Kind of a hack, get main and stick it in tenv  *)
  let hanoi_move = (List.nth_exn hanoi.functions 0) in

  let tenv =
    ((create_local_tenv (hanoi_move.declarations @ hanoi_move.parameters)),
     (create_top_tenv hanoi))
  in

  let ret_typ = hanoi_move.return_type in

  match t with
  | `Exp -> print_endline (show_typ (check_expr (parse expression s) tenv))
  | `Stm -> print_endline (show_statement (check_stmt (parse statement s) tenv ret_typ))

let%expect_test "id" =
  test_tc `Exp "peg2";
  [%expect{| (Parser.Struct "plate") |}]

let%expect_test "arithmetic expressions" =
  test_tc `Exp "1 + -numMoves";
  [%expect{| Parser.Int |}];

  test_tc `Exp "to / (from - 2)";
  [%expect{| Parser.Int |}]

let%expect_test "boolean expressions" =
  test_tc `Exp "false && true";
  [%expect{| Parser.Bool |}];

  test_tc `Exp "true || false";
  [%expect{| Parser.Bool |}]

let%expect_test "new array" =
  test_tc `Exp "new int_array[10]";
  [%expect{| Parser.Array |}]

let%expect_test "new struct" =
  test_tc `Exp "new plate";
  [%expect{| (Parser.Struct "plate") |}]

let%expect_test "invocation" =
  test_tc `Exp "printPeg(peg1)";
  [%expect{| Parser.Void |}]

let%expect_test "dot" =
  test_tc `Exp "peg1.plateUnder.plateUnder.plateUnder.size";
  [%expect{| Parser.Int |}]

let%expect_test "dot" =
  test_tc `Stm "return;";
  [%expect{| (Parser.Return None) |}]

let%expect_test "block" =
  test_tc `Stm "{ numMoves = 1 + 2; }";
  [%expect{|
    (Parser.Block
       [Parser.Assignment {
          target = (Parser.LVal { Parser.id = "numMoves"; left = None });
          source =
          (Parser.Expr (Parser.Add ((Parser.Integer 1), (Parser.Integer 2))))}
         ]) |}]

let%expect_test "loop" =
  test_tc `Stm "while (true || false) { print 1 endl; }";
  [%expect {|
    Parser.Loop {guard = (Parser.Or (Parser.True, Parser.False));
      body = [(Parser.PrintLn (Parser.Integer 1))]} |}]

let%expect_test "invocation statement" =
  test_tc `Stm "printPeg(peg1);";
  [%expect{|
    (Parser.InvocationS
       { Parser.id = "printPeg"; arguments = [(Parser.Id "peg1")] }) |}]

let%expect_test "conditional" =
  test_tc `Stm "if (true) { print 1 + 2; } else { print numMoves / 7; }";
  [%expect {|
    Parser.Conditional {guard = Parser.True;
      thn =
      [(Parser.Print (Parser.Add ((Parser.Integer 1), (Parser.Integer 2))))];
      els =
      (Some [(Parser.Print
                (Parser.Div ((Parser.Id "numMoves"), (Parser.Integer 7))))
              ])} |}]
