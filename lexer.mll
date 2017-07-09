

let digit = [ '0' - '9' ]
let alpha = [ 'A' - 'Z' 'a' - 'z' ]
let lower = [ 'a' - 'z' ]
let upper = [ 'A' - 'Z' ]
let param = upper (lower | upper | digit)*
let const = lower (lower | upper | digit)*
let space = ' ' | '\t' | '\r' | '\n'

let plfile = (lower | upper | digit)+ ".pl" 

rule token = parse
| space+      { token lexbuf   }
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
| "'"         { Parser.QUOTE   }
| eof         { Parser.EOF     }
| plfile as s { Parser.FILENAME s }
| const as c  { Parser.CONST c }
| param as p  { Parser.PARAM p }
| _           { failwith("Unknown Token: " ^ Lexing.lexeme lexbuf) }

