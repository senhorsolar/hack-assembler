open Ast
open Base
open Core
include Code
include Util

(* Extract AST expression from a single line *)
let parse (s : string) : expr =
    let s = (String.strip s) in
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast;;

(* Get list of expressions from a file *)
let readexprs filename = 
    List.map (nonemptylines filename) ~f:parse;;

(* Builds the symbol table in two passes *)
let build_symboltable exprs =
    let rec first_pass exprs symbol_table rom_addr = 
        match exprs with
        | [] -> symbol_table
        | (Label s)::t -> first_pass t (Map.add_exn symbol_table ~key:s ~data:rom_addr) rom_addr
        | (Adigit _ | Asymbol _ | Cinst _)::t ->
            first_pass t symbol_table (rom_addr + 1)
        | (Comment _)::t -> first_pass t symbol_table rom_addr in
    let rec second_pass exprs symbol_table ram_addr =
        match exprs with
        | [] -> symbol_table
        | (Asymbol s)::t when not (Map.mem symbol_table s)-> 
                second_pass t (Map.add_exn symbol_table ~key:s ~data:ram_addr) (ram_addr + 1)
        | _::t -> second_pass t symbol_table ram_addr in
    let first_symbol_table = first_pass exprs symbol_table 0 in
    let final_symbol_table = second_pass exprs first_symbol_table 16 in
    final_symbol_table;;

(* Translate list of expressions to binary *)
let translate expr_list symbol_table =
  let rec pass exprs res =
    match exprs with
    | [] -> Some (List.rev res)
    | (Adigit d)::t ->
       let bits = String.concat ["0"; dec2bin d 15] in
       pass t (bits::res)
    | (Asymbol s)::t ->
       let d = (Map.find_exn symbol_table s) in
       let bits = String.concat ["0"; dec2bin d 15] in
       pass t (bits::res)
    | (Cinst (d, c, j))::t ->
       (match (translateDest d, translateComp c, translateJump j) with
       |(Some d, Some c, Some j) ->
         let bits = String.concat ["111"; c; d; j] in
         pass t (bits::res)
       | _ -> None)
    | _::t -> pass t res in
  pass expr_list [];;
       
         
let assemble filename =
  let exprs = readexprs filename in
  let st = build_symboltable exprs in
  match (translate exprs st) with
  | None -> Out_channel.print_endline "Error while assembling file"
  | Some lines ->
     let outfilename = hackname filename in
     let oc = Out_channel.create outfilename in
     Out_channel.output_lines oc lines;
     Out_channel.close oc;;

let filename_param =
  let open Command.Param in
  anon ("filename" %: string);;

let command =
  Command.basic
    ~summary:"Translate <filename>.asm to <filename>.hack"
    ~readme: (fun () -> "Assembler for project 6 of Nand2Tetris")
    (Command.Param.map filename_param ~f:(fun filename ->
         (fun () -> assemble filename)));;

let () =
  Command.run command;;
