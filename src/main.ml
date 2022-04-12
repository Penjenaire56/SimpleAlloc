type src = string 
type dst = string
type op = Mul | Sum

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

let rec setupEnv (env : int SMap.t) (l : (string * int) list) = 
    match l with
    | [] -> env
    | (k,v) :: subL -> setupEnv (SMap.add k v env) subL 

let rec inter (prog : inst list) (env : int SMap.t) = 
    match prog with
    | [] -> env
    | Oper(op, d, s1, s2) :: subL -> 
        let s1 = SMap.find s1 env in
        let s2 = SMap.find s2 env in
        (match op with
        | Sum -> inter subL (SMap.add d (s1 + s2) env) 
        | Mul -> inter subL (SMap.add d (s1 * s2) env))
    | Move(d,s) :: subL ->
        inter subL (SMap.add d (SMap.find s env) env)
;;

let pprintInst (i : inst) = 
    match i with 
    | Oper(Mul , dst, src1, src2) -> dst ^ " = " ^ src1 ^ " * " ^ src2  
    | Oper(Sum , dst, src1, src2) -> dst ^ " = " ^ src1 ^ " + " ^ src2  
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
    let op = 
        if o = "+" then 
            Sum 
        else if o = "x" then
            Mul
        else
            failwith "Bad operator" 
    in  
    let v3 = List.nth line 4 in 
    (addVars (addVars (addVars v v1) v2) v3) , Oper(op, v1, v2, v3)
;;

let getRegistreNumber (regName : string) = 
    int_of_string (String.sub regName 1 ((String.length regName) - 1))

let read_lines name : string list =
    let ic = open_in name in
    let try_read () =
        try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s :: acc)
        | None -> close_in ic; List.rev acc in
    loop []
;;

(* Construit le programme à partir d'un fichier test *)
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

(* Donne les variables définies pendant une instruction *)
let defN = function
    | Move(d,_) | Oper(_,d,_,_) -> d

(* Donne les variables utilisés pendant une instruction *)
let useN = function
    | Move(_, s1) -> SSet.singleton s1
    | Oper(_, _, s1, s2) -> SSet.add s1 (SSet.singleton s2) 

let popS (newest : SSet.t) = 
    let n = SSet.choose newest in 
        (n, SSet.remove n newest) 

let popI (free : ISet.t) = 
    let n = ISet.choose free in 
        (n, ISet.remove n free) 

let getFresh free k = 
    if ISet.is_empty free then 
        (k + 1, free, k)
    else let (f,free) = popI free in 
        (k, free, f)

let rec aux_update newest k (regs : string IMap.t) (binds : int SMap.t) free = 
    if SSet.is_empty newest then
        (k, free, regs, binds)
    else 
        let (k, free, f) = getFresh free k in 
        let (n, newest) = popS newest in 
            aux_update newest k (IMap.add f n regs) (SMap.add n f binds) free

(* Mets à jour les binds et les variables définies *)
let update (regs : string IMap.t) (binds : int SMap.t) def newest free k = 
    let r = SMap.find def binds in
    if (SSet.cardinal newest) > 0 then
        let (n, newest) = popS newest in 
        let binds = SMap.add n r binds in  
        let binds = SMap.remove def binds in  
        let regs = IMap.add r n regs in  
            aux_update newest k regs binds free
    else   
        (k, ISet.add (SMap.find def binds) free, regs, binds)

let getRegistreName (var : string) (binds : int SMap.t) = 
    "r" ^ (string_of_int (SMap.find var binds))

let getRegistreNames (d,s1,s2) prevbinds binds = 
    let rdest = getRegistreName d prevbinds in 
    let rserc1 = getRegistreName s1 binds in 
    let rserc2 = getRegistreName s2 binds in 
        (rdest, rserc1, rserc2)

(* Construit les nouvelles instructions avec les registres générés automatiquement *)
let distribute (i : inst) binds (prevbinds : int SMap.t) (newest : SSet.t) regs k free = 
    match i with
    | Move(dst, src) -> 
        let instrs = [Move(getRegistreName dst prevbinds, getRegistreName src binds)] in 
            (instrs, k, free, regs, binds)

    | Oper(op, d, s1, s2) when (SSet.mem s1 newest) && (SSet.mem s2 newest) -> 
        let tmp1 = "tmp1" in 
        let tmp2 = "tmp2" in 
        let (k, free, regs, binds) = aux_update (SSet.add tmp2 (SSet.singleton tmp1)) k regs binds free in 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds binds in 
        let rtmp1 = getRegistreName tmp1 binds in 
        let rtmp2 = getRegistreName tmp2 binds in 
        let instrs = [
                Oper(op, rdest, rtmp1, rtmp2);
                Move(rtmp1, rserc1);
                Move(rtmp2, rserc2);
            ] in 
            (instrs, k, free, regs, binds)

    | Oper(op, d, s1, s2) when (SSet.mem s1 newest) -> 
        let tmp1 = "tmp1" in 
        let (k, free, regs, binds) = aux_update (SSet.singleton tmp1) k regs binds free in 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds binds in 
        let rtmp1 = getRegistreName tmp1 binds in 
        let instrs = [
            Oper(op, rdest, rtmp1, rserc2);
            Move(rtmp1, rserc1);
        ] in 
            (instrs, k, free, regs, binds)

    | Oper(op, d, s1, s2) when (SSet.mem s2 newest) -> 
        let tmp2 = "tmp2" in 
        let (k, free, regs, binds) = aux_update (SSet.singleton tmp2) k regs binds free in 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds binds in 
        let rtmp2 = getRegistreName tmp2 binds in 
        let instrs = [
            Oper(op, rdest, rserc1, rtmp2);
            Move(rtmp2, rserc2)
        ] in 
            (instrs, k, free, regs, binds)

    | Oper(op, d, s1, s2) -> 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds binds in 
        let instrs = [Oper(op, rdest, rserc1, rserc2)] in 
            (instrs, k, free, regs, binds)

let clean prog =
    List.filter (fun x -> 
        match x with 
        | Move(d,s) -> d <> s 
        | _ -> true
    ) prog 

(* Construit un nouveau programme avec les registres automatiquement alloués *)
let analyse (instr : inst list) (initials : SSet.t) = 
    let instr = List.rev instr in 

    let free = ISet.empty in 

    let (k, regs) = SSet.fold (fun s (k,m) -> k + 1, (IMap.add k s m)) initials (0, IMap.empty) in 

    let binds = IMap.fold (fun k v m -> SMap.add v k m) regs SMap.empty in

    let rec cross (currentVariables : SSet.t) (r : string IMap.t) (bind : int SMap.t) (k : int) (free : ISet.t) = function
    | [] -> bind, []
    | i :: subL -> 
        (* Variable déclaré pendant l'instruction *)
        let def = defN i in 
        
        (* Variable existant avant l'instruction *)
        let pre = SSet.diff currentVariables (SSet.singleton def) in 
        
        (* Variable utilisé pendant l'instruction *)
        let use = useN i in 

        (* Variable déclaré pendant l'instruction et étant nouvelle *)
        let newest = SSet.diff (SSet.diff use pre) currentVariables in 

        let previousBind = bind in 
        let (k,free,r,b) = update r bind def newest free k in 
        let (instrs, _, _, _, _) = distribute i b previousBind (SSet.diff use newest) r k free in 

        let prim, subL = cross (SSet.union pre use) r b k free subL in 

        prim , (instrs @ subL)
    in 
    let (bind, prog) = cross initials regs binds k free instr in 
    let optiProg = clean prog in (binds, bind, optiProg)

let rec setupTest (values : int list) (v : SSet.t) acc = 
    match values with
    | [] when SSet.is_empty v -> acc
    | x :: subL -> 
        let name = SSet.choose v in
        let v = SSet.remove name v in 
            setupTest subL v [(name, x)]
    | _ -> failwith "Inccorect init data"

let saveProg (path : string) prog = 
    let saveProg = open_out path in 
        List.iter (fun x -> output_string saveProg (pprintInst x ^ "\n")) prog

let getValueFromName (name : string) = 
    "(u64)[" ^ 
        (String.make 1 (String.get name 0)) ^ 
        " + 64 * " ^ 
        (String.sub name 1 ((String.length name) - 1)) ^ 
    "]"

let slpToJasmin dst variables out binds prog stack reg = 
    let header = "export fn p(reg u64 A, reg u64 B, reg u64 C) {\n" in 
    let footer = "\n}\n" in 
    let ret = SMap.fold (fun k v str -> str ^ "\t" ^ (getValueFromName k) ^ " = r" ^ (string_of_int v) ^ ";\n") out "" in 
    let declaration = 
        (List.fold_right (fun n str -> str ^ "\tstack u64 " ^ n ^ ";\n") 
            stack 
            (List.fold_right (fun n str -> str ^ "\treg u64 " ^ n ^ ";\n") reg "")) in 
            
    let assignation = SSet.fold 
        (fun x str -> "\tr" ^ (string_of_int (SMap.find x binds))
        ^ " = " ^ (getValueFromName x) 
        ^ ";\n" ^ str) (variables.var_in) "" 
    in

    let body = List.fold_right (fun x str -> str ^ "\t" ^ (pprintInst x ^ ";\n")) prog "" in 
    let saveProg = open_out dst in 
        output_string saveProg (header ^ declaration ^ assignation ^ body ^ ret ^ footer)


let findOccur (i : inst) = 
    match i with 
    | Move(dst,src) -> [dst; src]
    | Oper(_, dst,s1,s2) -> [dst; s1 ; s2]

let spilling (prog : inst list) (k : int) = 
    let rec spillingMapBuild (prog : inst list) = 
        match prog with 
        | [] -> SMap.empty
        | i :: subL -> 
            let m = spillingMapBuild subL in 
            let elements = findOccur i in 
            List.fold_right (fun e m -> 
                match SMap.find_opt e m with
                | None -> SMap.add e 0 m
                | Some(a) -> SMap.add e (a + 1) m
            ) elements m
    in 
    let m = SMap.fold (fun k v m -> 
        match IMap.find_opt v m with 
        | None -> (IMap.add v [k] m) 
        | Some(l) -> (IMap.add v (k::l) m)) 
        (spillingMapBuild prog) IMap.empty in 
    let regs = List.rev (IMap.bindings m) in 
    let rec split (name : string list) (i : int) accStack accReg = 
        match name with
            | [] -> (accStack, accReg)
            | x :: subL when i < k -> 
                split subL (i + 1) accStack (x :: accReg)
            | x :: subL -> 
                split subL (i + 1) (x :: accStack) accReg
    in split (List.fold_right (fun (_, l1) l2 -> l1 @ l2) regs []) 0 [] [] 

let stackAlloc (prog : inst list) (stack : string list) = 
    let progRebuild (i : inst) = 
        match i with
        | Oper(o , dst, s1, s2) when (List.mem dst stack) && (List.mem s1 stack) && (List.mem s2 stack) -> 
            [
                Move(dst, "tmp");
                Oper(o, "tmp", "tmp", s2);
                Move("tmp", s1);
            ]
        | Oper(o , dst, s1, s2) when (List.mem s1 stack) && (List.mem s2 stack) -> 
            [
                Oper(o, dst, s2, "tmp");
                Move("tmp", s1);
            ]
        | Oper(o , dst, s1, s2) when List.mem dst stack && List.mem s1 stack -> 
            [
                Move(dst, "tmp");
                Oper(o, "tmp", s2, s1);
            ]
        | Oper(o , dst, s1, s2) when List.mem dst stack -> 
            [
                Move(dst, "tmp");
                Oper(o, "tmp", s1, s2);
            ]
        | Oper(o , dst, s1, s2) when (List.mem s1 stack) -> 
            [
                Oper(o, dst, s2, s1);
            ]
        | i -> [i]
    in 
    let progReorganize (i : inst) = 
        match i with 
        | Oper(o , dst, s1, s2) when s2 == dst -> 
            Oper(o , dst, s2, s1)
        | _ -> i 
    in
    let rec aux_stackAlloc prog = 
        match prog with 
        | [] -> []
        | i :: subL -> 
           (match progRebuild (progReorganize i) with
                | [] -> aux_stackAlloc subL
                | [x] -> x :: (aux_stackAlloc subL)
                | res -> res @ (aux_stackAlloc subL)
           )
    in aux_stackAlloc prog

let build (k : int) (src : string) (dst : string) = 
    let (variables, instrs) = parse src in
    let (out, binds, newProg) = analyse instrs variables.var_out in 
    let (accStack, accReg) = spilling newProg (k - 1) in  
    let newProg = stackAlloc newProg accStack in 
        slpToJasmin dst variables out binds newProg accStack ("tmp" :: accReg) 

let testAlloc () = 
    let (variables, instrs) = parse "test/slp_630.txt" in 
    
    let env = setupEnv (SMap.empty) 
        [("A0", 1);("A1", 4);("A2", 5);("A3", 8);("A4", 9);("A5", 2);
        ("B0", 2);("B1", 3);("B2", 6);("B3", 2);("B4", 4);("B5", 0);] 
    in 

    Printf.eprintf "Interprétation sans allocation \n"; 
    let exec = inter instrs env in 
        SMap.iter (fun x v -> Printf.eprintf "%s " (x ^ " = " ^ (string_of_int v))) (SMap.filter (fun x _ -> (String.contains x 'C')) exec);
        Printf.eprintf "\n";

    let (_, _, newProg) = analyse instrs variables.var_out in saveProg "test/slp_630_alloc.txt" newProg;
    
    Printf.eprintf "Interprétation avec allocation \n"; 
    let env = setupEnv (SMap.empty) 
        [("r21", 1);("r24", 4);("r7", 5);("r12", 8);("r11", 9);("r8", 2);
        ("r18", 2);("r28", 3);("r29", 6);("r2", 2);("r17", 4);("r27", 0);] 

    in let exec = inter newProg env in 
        SMap.iter (fun x v -> Printf.eprintf "%s " (x ^ " = " ^ (string_of_int v))) (SMap.filter (fun x _ -> (getRegistreNumber x) < 6) exec);
        Printf.eprintf "\n"

let main () = 
    build 14 "test/slp_630.txt" "test/slp_630.jazz"
;;

main ()