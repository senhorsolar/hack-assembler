open Base

let symbol_table = Map.of_alist_exn (module String)
                     ["SP", 0;   
                      "LCL", 1;
                      "ARG", 2;
                      "THIS", 3;
                      "THAT", 4;
                      "R0", 0;
                      "R1", 1;
                      "R2", 2;
                      "R3", 3;
                      "R4", 4;
                      "R5", 5;
                      "R6", 6;
                      "R7", 7;
                      "R8", 8;
                      "R9", 9;
                      "R10", 10;
                      "R11", 11;
                      "R12", 12;
                      "R13", 13;
                      "R14", 14;
                      "R15", 15;
                      "SCREEN", 16484;
                      "KBD", 24576];;

let translateComp c = match c with
      | "0" -> Some "0101010"
      | "1" -> Some "0111111"
      | "-1" -> Some "0111010"
      | "D" -> Some "0001100"
      | "A" -> Some "0110000"
      | "!D" -> Some "0001101"
      | "!A" -> Some "0110001"
      | "-D" -> Some "0001111"
      | "-A" -> Some "0110011"
      | "D+1" -> Some "0011111"
      | "A+1" -> Some "0110111"
      | "D-1" -> Some "0001110"
      | "A-1" -> Some "0110010"
      | "D+A" -> Some "0000010"
      | "D-A" -> Some "0010011"
      | "A-D" -> Some "0000111"
      | "D&A" -> Some "0000000"
      | "D|A" -> Some "0010101"
      | "M" -> Some "1110000"
      | "!M" -> Some "1110001"
      | "M+1" -> Some "1110111"
      | "M-1" -> Some "1110010"
      | "D+M" -> Some "1000010"
      | "D-M" -> Some "1010011"
      | "M-D" -> Some "1000111"
      | "D&M" -> Some "1000000"
      | "D|M" -> Some "1010101"
      | _ -> None;;

let translateDest d = match d with
  | None -> Some "000"
  | Some d -> match d with
              | "M" -> Some "001"
              | "D" -> Some "010"
              | "MD" -> Some "011"
              | "A" -> Some "100"
              | "AM" -> Some "101"
              | "AD" -> Some "110"
              | "AMD" -> Some "111"
              | _ -> None;;

let translateJump j = match j with
  | None -> Some "000"
  | Some j -> match j with
              | "JGT" -> Some "001"
              | "JEQ" -> Some "010"
              | "JGE" -> Some "011"
              | "JLT" -> Some "100"
              | "JNE" -> Some "101"
              | "JLE" -> Some "110"
              | "JMP" -> Some "111"
              | _ -> None;;

