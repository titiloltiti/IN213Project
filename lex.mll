{
  open Parse;;
  exception Eoi ;;
}

let newline = ('\010' | '\013' | "\013\010")

rule prochain_lexeme = parse
    (' ' | '\t' | newline )
      { prochain_lexeme lexbuf }  
  | ['0'-'9']+ as lxm
      { INT(int_of_string lxm) }
  | ('+'|'-')?['0'-'9']+'.'['0'-'9']* as lxm { FLOAT(float_of_string lxm) }
  | [ 'A'-'Z' 'a'-'z' ] ('_' |['0'-'9']|[ 'A'-'Z' 'a'-'z' ])* as lxm
      { match lxm with
        | "let" -> LET
        | "in" -> IN
        | "add" -> ADD
        | "fun" -> FUN
        | "remove" -> REMOVE
        | "Sphere" -> SPHERE
        | "Eye" -> EYE
        | "Source" -> SOURCE
        | "Plane" -> PLANE
        | _ -> IDENT(lxm)
      }
  | '='    { EGAL }
  | '+'    { PLUS }
  | '-'    { MINUS }
  | '*'    { TIMES }
  | '/'    { DIV }
  | ';'    { SEMICOLON }
  | ','    { COMMA }
  | '.'    { DOT }
  | '('    { LPAREN }
  | ')'    { RPAREN }
  | "->"   { ARROW }
  | "//"   { in_cpp_comment lexbuf }
  | "/*"   { in_c_comment lexbuf }
  | eof    { raise Eoi }
  | _  as c { Printf.eprintf "Invalid char `%c'\n%!" c ; prochain_lexeme lexbuf }

and in_cpp_comment = parse
    '\n' { prochain_lexeme lexbuf }
  | _    { in_cpp_comment lexbuf }
  | eof  { raise Eoi }

and in_c_comment = parse
    "*/" { prochain_lexeme lexbuf }
  | _    { in_c_comment lexbuf }
  | eof  { raise (Failure "Commentaire non termine") }
