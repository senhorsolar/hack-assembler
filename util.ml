open Base
open Stdio

let isempty line = 
    match (String.strip line) with
    | "" -> true
    | _ -> false;;

let notempty line = not (isempty line);;

let nonemptylines filename = 
    List.filter (In_channel.read_lines filename) ~f:notempty;;

(* Convert decimal number < 2**15 to its binary representation *)
let dec2bin (dec : int) nbits : string = 
    let pad s len = 
        let slen = String.length s in
        let padlen = max 0 (len - slen) in
        let prefix = String.init padlen ~f:(fun _ -> '0') in 
        String.concat [prefix; s] in
    let rec helper ls d = 
        if (List.length ls) = nbits then ls
        else
            match d with
            | 1 -> '1'::ls
            | 0 -> '0'::ls
            | x -> if (x % 2 = 0) then helper ('0'::ls) (d / 2)
                   else helper ('1'::ls) (d / 2) in
    let ls = helper [] dec in
    let s = String.of_char_list ls in
    pad s nbits;;

let hackname filename =
  String.concat [Caml.Filename.remove_extension filename; ".hack"];;
