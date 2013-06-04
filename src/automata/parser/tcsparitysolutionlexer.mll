(* File tcsparitysolutionlexer.mll *)

{

open Tcsparitysolutionparser ;;        (* The type token is defined in tcsparitysolutionparser.mli *)
open Tcsgameparserinternal ;;

}

rule token = parse
    ['\n']                             { incr __lexer_line; __lexer_character := 0; token lexbuf }
  | [' ' '\t']                         { incr __lexer_character; token lexbuf }
  | ['0'-'9']+ as lxm                  { __lexer_character := !__lexer_character + (String.length lxm); INT(int_of_string lxm) }
  | ';'                                { incr __lexer_character; SEMICOLON }
  | eof                                { incr __lexer_character; EOF }
  | "paritysol"                        { __lexer_character := !__lexer_character + 9; PARITYSOL }
