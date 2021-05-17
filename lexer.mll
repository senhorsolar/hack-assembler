{
    open Parser
}

(* Some primitives *)
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let dot = ['.']
let dollar = ['$']
let white = [' ' '\t']+
let comment = "//" _*
let newline = '\n' | '\r' | "\r\n"

(* For A instructions *)
let symbol = (letter | '_' | dot | dollar | ':')+ 
             (letter | digit | '_' | dot | dollar | ':')*
let uint = (digit | ['1'-'9'] digit*)

(* For C instructions *)
let comp =
     "0" | "1" | "-1"
    | "D" | "A" | "!D"| "!A" | "-D" | "-A"
    | "D+1" | "A+1" | "D-1" | "A-1"
    | "D+A" | "D-A" | "A-D" | "D&A" | "D|A"
    | "M" | "!M" | "-M" | "M+1" | "M-1"
    | "D+M" | "D-M" | "M-D" | "D&M" | "D|M"
let dest = "M" | "D" | "MD" | "A" | "AM" | "AD" | "AMD"
let jump = "JGT" | "JEQ" | "JGE" | "JLT" | "JNE" | "JLE" | "JMP"

rule read = 
    parse
    | white { read lexbuf }
    | comment as c { COMMENT (c) }
    | "@" (symbol as value) { ASYMBOL (value) }
    | "@" (uint as value) { ADIGIT (int_of_string value) }
    | ((dest as d) '=')? (comp as c) (';' (jump as j))? { CINST (d, c, j) }
    | "(" (symbol as s) ")" { LABEL (s) }
    | eof { EOF }
