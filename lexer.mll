

let digit = [ '0' - '9' ]
let alpha = [ 'A' - 'Z' 'a' - 'z' ]
let lower = [ 'a' - 'z' ]
let upper = [ 'A' - 'Z' ]
let param = ('_' | upper) ('_' | lower | upper | digit)*
let const = (digit | lower) ('_' | lower | upper | digit)*
let space = ' ' | '\t' | '\r' | '\n'

let plfile = (lower | upper | digit)+ ".pl" 

rule token = parse
| space+      { token lexbuf   }
| "/*"        { comment lexbuf }
| "#"         { Parser.SHARP   }
| "load"      { Parser.LOAD    }
| "list"      { Parser.LIST    }
| "("         { Parser.LPAR    }
| ")"         { Parser.RPAR    }
| ":-"        { Parser.IF      }
| "."         { Parser.COLON   }
| ","         { Parser.COMMA   }
| "["         { Parser.BLPAR   }
| "]"         { Parser.BRPAR   }
| "|"         { Parser.VERT    }
| "'"         { Parser.QUOTE   }
| "!"         { Parser.CUT     }
| eof         { Parser.EOF     }
| plfile as s { Parser.FILENAME s }
| const as c  { Parser.CONST c }
| param as p  { Parser.PARAM p }
| _           { failwith("Unknown Token: " ^ Lexing.lexeme lexbuf) }

and comment = parse
| "*/"       { token lexbuf }
| _          { comment lexbuf }
