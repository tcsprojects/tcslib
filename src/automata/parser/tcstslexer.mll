(* File tcstslexer.mll *)

{

open Tcstsparser ;;        (* The type token is defined in tcstsparser.mli *)
open Tcstransitionsysparserinternal ;;

}

rule token = parse
    ['\n']                             { incr __lexer_line; __lexer_character := 0; token lexbuf }
  | [' ' '\t']                         { incr __lexer_character; token lexbuf }
  | ['0'-'9']+ as lxm                  { __lexer_character := !__lexer_character + (String.length lxm); INT(int_of_string lxm) }
  | ','                                { incr __lexer_character; COMMA }
  | ';'                                { incr __lexer_character; SEMICOLON }
  | '"'((_ # ['"' '\n'])* as lxm)'"'   { __lexer_character := !__lexer_character + (String.length lxm) + 2; ANN(lxm) }
  | eof                                { incr __lexer_character; EOF }
  | "ts"                               { __lexer_character := !__lexer_character + 2; TS }
  | "start"                            { __lexer_character := !__lexer_character + 5; START }
  | (['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as lxm) { __lexer_character := !__lexer_character + (String.length lxm) ; STRING(lxm) }
