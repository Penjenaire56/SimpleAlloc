type src = string 
type dst = string
type op = string

type inst = 
| Oper of op * dst * src * src
| Move of dst * src

module ISet = Set.Make(Int)
module SSet = Set.Make(String)
module IMap = Map.Make(Int)
module SMap = Map.Make(String)

type vars = {
    var_in : SSet.t;
    var_out : SSet.t
}

let pprintInst (i : inst) = 
    match i with 
    | Oper(op, dst, src1, src2) -> dst ^ " = " ^ src1 ^ " " ^ op ^ " " ^ src2  
    | Move(dst,src) -> dst ^ " = " ^ src
;;


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
    let o = List.nth line 3 in 
    let v3 = List.nth line 4 in 
    (addVars (addVars (addVars v v1) v2) v3) , Oper(o, v1, v2, v3)
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

let defN = function
    | Move(d,_) | Oper(_,d,_,_) -> d

let useN = function
    | Move(_, s1) -> SSet.singleton s1
    | Oper(_, _, s1, s2) -> SSet.add s1 (SSet.singleton s2) 

let rec aux_update newest k (regs : string IMap.t) (binds : int SMap.t) free = 
    if SSet.is_empty newest then
        (k, free, regs, binds)
    else if not (ISet.is_empty free) then 
        let n = SSet.choose newest in 
        let newest = SSet.remove n newest in 
        let f = ISet.choose free in
        let free = ISet.remove f free in
            aux_update newest k (IMap.add f n regs) (SMap.add n f binds) free
    else
        let n = SSet.choose newest in 
        let newest = SSet.remove n newest in 
            aux_update newest (k + 1) (IMap.add k n regs) (SMap.add n k binds) free

let update (regs : string IMap.t) (binds : int SMap.t) def newest free k = 
    let r = SMap.find def binds in
    if (SSet.cardinal newest) > 0 then
        let n = SSet.choose newest in 
        let newest = SSet.remove n newest in 
        let binds = SMap.add n r binds in  
        let binds = SMap.remove def binds in  
        let regs = IMap.add r n regs in  
            aux_update newest k regs binds free
    else   
        (k, ISet.add (SMap.find def binds) free, regs, binds)

let distribute (i : inst) (binds : int SMap.t) (prevbinds : int SMap.t) = 
    let pf = (fun x -> SMap.find x prevbinds) in
    let pg = (fun x -> "r" ^ string_of_int x) in  
    let ph = (fun x -> pg (pf x)) in 

    let f = (fun x -> SMap.find x binds) in
    let g = (fun x -> "r" ^ string_of_int x) in  
    let h = (fun x -> g (f x)) in 
        match i with
        | Move(dst, src) -> Move(ph dst, h src)
        | Oper(op, d, s1, s2) -> Oper(op, ph d, h s1, h s2)




let analyse (instr : inst list) (initials : SSet.t) = 
    let instr = List.rev instr in 

    let free = ISet.empty in 

    let (k, regs) = SSet.fold (fun s (k,m) -> k + 1, (IMap.add k s m)) initials (0, IMap.empty) in 

    let binds = IMap.fold (fun k v m -> SMap.add v k m) regs SMap.empty in

    let rec cross v (r : string IMap.t) (b : int SMap.t) (k : int) (free : ISet.t) = function
    | [] -> []
    | x :: subL -> 
        let def = defN x in 
        let pre = SSet.diff v (SSet.singleton def) in 
        let use = useN x in 
        let newest = SSet.diff use pre in 
        let pbind = b in 
        let (k,free,r,b) = update r b def newest free k in 
        let ninst = (distribute x b pbind) in 
        ninst :: (cross (SSet.union pre use) r b k free subL)

    in cross initials regs binds k free instr 

let main() = 
    let (variables, instrs) = parse "test/slp_630.txt" in 
    let prog = analyse instrs variables.var_out in 

    List.iter (fun x -> Printf.eprintf "%s \n" (pprintInst x)) (List.rev prog) 

;;

main()