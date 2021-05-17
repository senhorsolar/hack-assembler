open Ast

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

(* let buildsymbol_table = 
TODO
    *)

(* 

(* let translate expr_list symbol_table = 
TODO
    *)

(* let extract_filename filename = 
TODO
    *)

(* let list_to_file ls = 
TODO
    *)
