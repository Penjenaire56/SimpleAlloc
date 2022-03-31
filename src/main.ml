type src = string 
type dst = string

type inst = 
| Oper of dst * src * src
| Move of dst * src

module SSet = Set.Make(String)

type vars = {
    var_in : SSet.t;
    var_out : SSet.t
}

let addVars (v : vars) (n : string) = 
    if (String.get n 0) = 'C' then {
        var_in = v.var_in;
        var_out = SSet.add n (v.var_out);
    } else if (String.get n 0) = 'A' || (String.get n 0) = 'B' then {
        var_in = SSet.add n (v.var_in);
        var_out = v.var_out;
    }
    else  v
;;

let getMove (line : string list) (v : vars) = 
    let v1 = List.nth line 0 in 
    let v2 = List.nth line 2 in 
    (addVars (addVars v v1) v2) , Move(v1 , v2) 
;;

let getOper (line : string list) (v : vars) = 
    let v1 = List.nth line 0 in 
    let v2 = List.nth line 2 in 
    let v3 = List.nth line 4 in 
    (addVars (addVars (addVars v v1) v2) v3) , Oper(v1, v2, v3)
;;

let read_lines name : string list =
    let ic = open_in name in
    let try_read () =
        try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s :: acc)
        | None -> close_in ic; List.rev acc in
    loop []
;;

let parse (path : string) = 
    let lines = read_lines path in

    let rec parse_line v = function
    | [] -> v , []
    | x :: subL -> 
        let line = String.split_on_char ' ' x in 
        let len = List.length line in
            if len = 5 then
                let (v,i) = (getOper line v) in
                let (v,l) = parse_line v subL in 
                (v, i :: l)
            else if len = 3 then 
                let (v,i) = (getMove line v) in
                let (v,l) = parse_line v subL in 
                (v, i :: l)
            else 
                parse_line v subL

        in parse_line { var_in = SSet.empty ; var_out = SSet.empty } lines
;;



let main() = 
    let (variables, instrs) = parse "test/slp_630.txt" in 
    
    Printf.eprintf "in : %d\n" (SSet.cardinal variables.var_in);
    Printf.eprintf "out : %d\n" (SSet.cardinal variables.var_out);
    Printf.eprintf "instr : %d\n" (List.length instrs);
;;

main()