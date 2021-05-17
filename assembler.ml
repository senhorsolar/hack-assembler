open Ast
open Base
open Code

let readlines filename = 
    let ic = open_in filename in
    let try_read () = 
        try
            Some (input_line ic)
        with End_of_file -> close_in ic; None in
        let rec loop acc = match try_read () with
            | Some s -> loop (s :: acc)
            | None -> List.rev acc in
        loop [];;

let isempty line = 
    match (String.trim line) with
    | "" -> true
    | _ -> false;;

let notempty line = not (isempty line);;

let nonemptylines filename = 
    List.filter notempty (readlines filename);;

let parse (s : string) : expr =
    let s = (String.trim s) in
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast;;

let readexprs filename = 
    List.map parse (nonemptylines filename);;

let buildsymbol_table exprs =
    let rec first_pass exprs symbol_table rom_addr = 
        match exprs with
        | [] -> symbol_table
        | h::t -> 
                match h with
                | Label s -> first_pass t (Map.add_exn symbol_table s rom_addr) rom_addr
                | Adigit _ | Asymbol _ | Cinst _ -> 
                        first_pass t symbol_table (rom_addr + 1)
                | Comment _ | _ -> first_pass t symbol_table rom_addr in
    let rec second_pass exprs symbol_table ram_addr =
        match exprs with
        | [] -> symbol_table
        | h::t ->
                match h with
                | Asymbol s ->
                        match (Map.find symbol_table s) with
                        | None -> second_pass t (Map.add_exn symbol_table s
                        ram_addr) (ram_addr + 1)
                        | Some _ -> second_pass t symbol_table ram_addr
                | _ -> second_pass t symbol_table ram_addr in
    let first_symbol_table = first_pass exprs Code.symbol_table 0 in
    let final_symbol_table = second_pass exprs first_symbol_table 16 in
    final_symbol_table;;
 

(* let translate expr_list symbol_table = 
TODO
    *)

(* let extract_filename filename = 
TODO
    *)

(* let list_to_file ls = 
TODO
    *)
