(* File tcsautomatalexer.mll *)

{

open Tcsautoparser ;;        (* The type token is defined in tcsautoparser.mli *)
open Tcsautomataparserinternal ;;

}

rule token = parse
    ['\n']                             { incr __lexer_line; __lexer_character := 0; token lexbuf }
  | [' ' '\t']                         { incr __lexer_character; token lexbuf }
  | ['0'-'9']+ as lxm                  { __lexer_character := !__lexer_character + (String.length lxm); INT(int_of_string lxm) }
  | ','                                { incr __lexer_character; COMMA }
  | '('                                { incr __lexer_character; BRACKETOPEN }
  | ')'                                { incr __lexer_character; BRACKETCLOSE }
  | '<'                                { incr __lexer_character; ALPHABETCALL }
  | '>'                                { incr __lexer_character; ALPHABETRET }
  | '_'                                { incr __lexer_character; UNDERSCORE }
  | ';'                                { incr __lexer_character; SEMICOLON }
  | '§'                                { incr __lexer_character; EPSILON }
  | '"'((_ # ['"' '\n'])* as lxm)'"'   { __lexer_character := !__lexer_character + (String.length lxm) + 2; ANN(lxm) }
  | eof                                { incr __lexer_character; EOF }
  | "automaton"						   { __lexer_character := !__lexer_character + (String.length "AUTOMATON"); AUTOMATON }
  | "alphabet"                         { __lexer_character := !__lexer_character + (String.length "ALPHABET"); ALPHABET }
  | "states"                           { __lexer_character := !__lexer_character + (String.length "STATES"); STATES }
  | "initial"                          { __lexer_character := !__lexer_character + (String.length "INITIAL"); INITIAL }
  | "transitions"                      { __lexer_character := !__lexer_character + (String.length "TRANSITIONS"); TRANSITIONS }
  | "stack"                            { __lexer_character := !__lexer_character + (String.length "STACK"); STACK }
