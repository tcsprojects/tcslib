open Tcsformulaparser;;
open Tcsformulalexer;;
open Printf;;
open Tcsmetaformula;;

let parse s =
  let buff = Lexing.from_string(s)
in
  try
    Tcsformulaparser.program Tcsformulalexer.lexer buff
  with
    Failure s -> print_string ("Parser failure :: " ^ s ^ "\n"); exit 0;
  | Tcsformulalexer.Eof -> print_string "Parser failure :: End Of String\n"; exit 0;;

let parse_expression s =
  match (parse ("#dummydef := " ^ s ^ ";")) with
    ("#dummydef", [], [(CondTT, expr)])::[] -> expr
  | _ -> failwith("String '" ^ s ^ "' is not an expression!");;