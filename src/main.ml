type src = string 
type dst = string
type op = Mul | Sum

type inst = 
| Oper of op * dst * src * src
| Move of dst * src
| In of dst * string
| Out of src * string


module ISet = Set.Make(Int)
module SSet = Set.Make(String)
module IMap = Map.Make(Int)
module SMap = Map.Make(String)

type vars = {
    var_in : SSet.t;
    var_out : SSet.t
}

type memory = {
    k : int;
    max : int;
    free : int IMap.t;
    usage : int IMap.t;
    regs : string IMap.t;
    binds : int SMap.t;
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
    | In(_) :: subL ->
        inter subL env
    | Out(_) :: subL -> 
        inter subL env

let getValueFromName (name : string) = 
    "(u64)[" ^ 
        (String.make 1 (String.get name 0)) ^ 
        " + 8 * " ^ 
        (String.sub name 1 ((String.length name) - 1)) ^ 
    "]"

let getIndiceFromName (name : string) = 
    (String.make 1 (String.get name 0)) ^ "[" ^ (String.sub name 1 ((String.length name) - 1)) ^ "]"

let pprintInst (i : inst) = 
    match i with 
    | Oper(Mul , dst, src1, src2) -> dst ^ " = " ^ src1 ^ " & " ^ src2  
    | Oper(Sum , dst, src1, src2) -> dst ^ " = " ^ src1 ^ " ^ " ^ src2  
    | Move(dst,src) -> dst ^ " = " ^ src
    | In(dst, name) -> dst ^ " = " ^ getValueFromName name
    | Out(src, name) -> getValueFromName name ^ " = " ^ src

let addVars (v : vars) (n : string) = 
    if (String.get n 0) = 'C' || (String.get n 0) = 'c' then {
        var_in = v.var_in;
        var_out = SSet.add n (v.var_out);
    } else if (String.get n 0) = 'A' || (String.get n 0) = 'B' then {
        var_in = SSet.add n (v.var_in);
        var_out = v.var_out;
    } else if (String.get n 0) = 'a' || (String.get n 0) = 'b' then {
        var_in = SSet.add n (v.var_in);
        var_out = v.var_out;
    }
    else  v

let getMove (line : string list) (v : vars) = 
    let v1 = List.nth line 0 in 
    let v2 = List.nth line 2 in 
    (addVars (addVars v v1) v2) , Move(v1 , v2) 

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

let rec aux_reduce (instrs : inst list) =
    match instrs with
        | [] -> []
        | i :: subI -> (
            match i with
            | Oper(_, dst, _,_) | In(dst,_) | Move(dst,_) -> 
                aux_insert subI dst i
            | _ ->  
                i :: (aux_reduce subI)
        )  

and aux_insert (instrs : inst list) (name : string) (x : inst) = 
    match instrs with
    | [] -> [] 
    | i :: subI -> (
        match i with
        | Oper(_, _, s1, s2) when s1 = name || s2 = name -> 
            x :: i :: aux_reduce subI
        | Move(_, s) when s = name -> 
            x :: i :: subI
        | Out(s,_) when s = name -> 
            x :: i :: subI
        | _ -> 
            i :: (aux_insert subI name x)
    )

let reduce (instrs : inst list) = 
    let rec aux instrs k = 
        if k > 0 then
            aux (aux_reduce instrs) (k - 1) 
        else
            instrs
    in aux instrs 1000

(* Donne les variables définies pendant une instruction *)
let defN = function
    | Out(_,_) -> None
    | In(d,_) 
    | Move(d,_) 
    | Oper(_,d,_,_) -> Some(d)

(* Donne les variables utilisés pendant une instruction *)
let useN = function
    | Move(_, s1) -> SSet.singleton s1
    | Oper(_, _, s1, s2) -> SSet.add s1 (SSet.singleton s2) 
    | _ -> SSet.empty

let mem_bind (mem : memory) (n : string) (r : int) = {
    k = mem.k;
    max = mem.max;
    free = mem.free;
    usage = mem.usage;
    binds = SMap.add n r mem.binds;
    regs = IMap.add r n mem.regs;
}

let mem_free (mem : memory) (v : string) = 
    let n = SMap.find v mem.binds in 
    let v = IMap.find n mem.usage in {
    k = mem.k;
    max = mem.max;
    free = IMap.add n v mem.free;
    usage = IMap.remove n mem.usage;
    binds = mem.binds;
    regs = mem.regs;
}

let popS (newest : SSet.t) = 
    let n = SSet.choose newest in 
        (n, SSet.remove n newest) 

let getFresh (mem : memory) n = 
    if IMap.is_empty mem.free then 
    { 
        k = mem.k + 1;
        max = mem.max;
        free = mem.free;
        usage = IMap.add mem.k 0 mem.usage;
        binds = SMap.add n mem.k mem.binds;
        regs = IMap.add mem.k n mem.regs;
    } 
    else let (k, v) = IMap.choose mem.free in {
        k = mem.k;
        max = mem.max;
        free = IMap.remove k mem.free;
        usage = IMap.add k v mem.usage;
        binds = SMap.add n k mem.binds;
        regs = IMap.add k n mem.regs;
    }

let rec aux_update newest (mem : memory) = 
    if SSet.is_empty newest then
        mem
    else 
        let (n, newest) = popS newest in 
            aux_update newest (getFresh mem n)

(* Mets à jour les binds et les variables définies *)
let update def newest (mem : memory) = 
    match def with
    | None -> aux_update newest mem 
    | Some(def) -> 
        aux_update newest (mem_free mem def)

let getRegistreName (var : string) (binds : int SMap.t) = 
    "r" ^ (string_of_int (SMap.find var binds))

let getRegistreNames (d,s1,s2) prevbinds binds = 
    let rdest = getRegistreName d prevbinds in 
    let rserc1 = getRegistreName s1 binds in 
    let rserc2 = getRegistreName s2 binds in 
        (rdest, rserc1, rserc2)

(* Construit les nouvelles instructions avec les registres générés automatiquement *)
let distribute (i : inst) (prevbinds : int SMap.t) (newest : SSet.t) mem = 
    match i with
    | Move(dst, src) -> 
        [Move(getRegistreName dst prevbinds, getRegistreName src mem.binds)] , mem
    | In(src, name) -> 
        [In(getRegistreName src mem.binds, name)] , mem
    | Out(dst, name) -> 
        [Out(getRegistreName dst mem.binds, name)] , mem
    | Oper(op, d, s1, s2) when (SSet.mem s1 newest) && (SSet.mem s2 newest) -> 
        let tmp1 = "tmp1" in 
        let mem = aux_update (SSet.singleton tmp1) mem in 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds mem.binds in 
        let rtmp1 = getRegistreName tmp1 mem.binds in 
        [
            Oper(op, rdest, rtmp1, rserc2);
            Move(rtmp1, rserc1);
        ] ,
        mem_free mem tmp1 
    | Oper(op, d, s1, s2) when (SSet.mem s1 newest) -> 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds mem.binds in 
        [
            Oper(op, rdest, rserc2, rserc1);
        ] , mem
    | Oper(op, d, s1, s2) -> 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds mem.binds in 
            [Oper(op, rdest, rserc1, rserc2)] , mem

let clean prog =
    List.filter (fun x -> 
        match x with 
        | Move(d,s) -> d <> s 
        | _ -> true
    ) prog 

let update_usage (mem : memory) (vars : SSet.t) = {
    k = mem.k;
    max = mem.max;
    free = mem.free;
    usage = 
        SSet.fold (fun s u -> 
            let k = (SMap.find s mem.binds) in
            let v = IMap.find_opt k u in 
            match v with
            | None -> u 
            | Some(v) -> (IMap.add k (v + 1) u)
    ) vars mem.usage;
    binds = mem.binds;
    regs = mem.regs;
}

let init (initials : SSet.t) (max : int) = (* TODO stack if enougth variables *)
    let (k, regs, usage) = SSet.fold 
        (fun s (k,m,u) -> k + 1, (IMap.add k s m), (IMap.add k 0 u)) 
        initials (0, IMap.empty, IMap.empty) 
    in let binds = IMap.fold (fun k v m -> SMap.add v k m) regs SMap.empty 
    in {
        k = k;
        max = max;
        free = IMap.empty; 
        usage = usage;
        regs = regs;
        binds = binds;
    }

(* Construit un nouveau programme avec les registres automatiquement alloués *)
let analyse (instr : inst list) (initials : SSet.t) (max : int) = 
    let instr = List.rev instr in 

    let mem = init initials max in
    
    let rec cross (variables : SSet.t) (mem : memory) = function
    | [] -> 
        let acc = (IMap.bindings mem.usage) @ (IMap.bindings mem.free) in 
        let acc = List.sort (fun (_,x) (_,y) -> Int.compare x y) acc in 
        let acc = List.rev acc in 

        let rec split k = function 
        | [] -> [],[]
        | (x,_) :: subL -> 
            let (l1,l2) = split (k - 1) subL in 
                if k > 0 then 
                    (x :: l1, l2)
                else 
                    (l1, x :: l2)
        in

        let (regAcc, stackAcc) = split mem.max acc in
        (List.map (fun n -> "r" ^ (string_of_int n)) regAcc, List.map (fun n -> "r" ^ (string_of_int n)) stackAcc) , []
    | i :: subL -> 
        (* Variable déclaré pendant l'instruction *)
        let def = defN i in 
        
        (* Variable existant avant l'instruction *)
        let defopt = match def with 
            | None -> SSet.empty
            | Some(s) -> SSet.singleton s in 
        let pre = SSet.diff variables defopt in 

        (* Variable utilisé pendant l'instruction *)
        let use = useN i in 

        (* Variable déclaré pendant l'instruction et étant nouvelle *)
        let newest = SSet.diff (SSet.diff use pre) variables in 

        let previousBind = mem.binds in 
        let mem = update def newest mem in 

        let mem = update_usage mem use in 
        let instrs, mem = distribute i previousBind (SSet.diff use newest) mem in 

        let (regAcc, stackAcc) , subL = cross (SSet.union pre use) mem subL in 
            (regAcc, stackAcc) , instrs @ subL
    in 
    let (regAcc, stackAcc), prog = cross initials mem instr in 
    let optiProg = clean prog in (regAcc, stackAcc), optiProg

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

let slpToJasmin dst prog stack reg = 
    let header = "export fn p(reg u64 A, reg u64 B, reg u64 C) {\n" in 
    let footer = "\n}\n" in 
    let declaration = 
        (List.fold_right (fun n str -> str ^ "\tstack u64 " ^ n ^ ";\n") 
            stack 
            (List.fold_right (fun n str -> str ^ "\treg u64 " ^ n ^ ";\n") reg "")) in 
            
    let body = List.fold_right (fun x str -> str ^ "\t" ^ (pprintInst x ^ ";\n")) prog "" in 
    
    let saveProg = open_out dst in 
        output_string saveProg (header ^ declaration ^ body ^ footer)

let stackAlloc (i : inst) (stack : SSet.t) = 
    match i with
    | Oper(o , dst, s1, s2) when (SSet.mem dst stack) && (SSet.mem s1 stack) && (SSet.mem s2 stack) -> 
        [
            Move("tmp", s1);
            Oper(o, "tmp", "tmp", s2);
            Move(dst, "tmp");
        ]
    | Oper(o , dst, s1, s2) when SSet.mem s1 stack && SSet.mem s2 stack -> 
        [
            Move("tmp", s1);
            Oper(o, dst, "tmp", s2);
        ]
    | Oper(o , dst, s1, s2) when SSet.mem dst stack && SSet.mem s1 stack -> 
        [
            Move("tmp", s1);
            Oper(o, "tmp", "tmp", s2);
            Move(dst, "tmp");
        ]
    | Oper(o , dst, s1, s2) when SSet.mem dst stack -> 
        [
            Oper(o, "tmp", s1, s2);
            Move(dst, "tmp");
        ]
    | Oper(o , dst, s1, s2) when SSet.mem s1 stack -> 
        [
            Oper(o, dst, s2, s1);
        ]
    | i -> [i]
    

let buildIn (var_in : SSet.t) = 
    let rec _buildIn (var_in : SSet.t) acc = 
    if SSet.is_empty var_in then
        acc
    else
        let v = SSet.choose var_in in 
        let var_in = SSet.remove v var_in in 
            _buildIn var_in (In(v, v) :: acc)
    in _buildIn var_in []

let rec insertOut (prog : inst list) (var_out : SSet.t) = 
    let rec insertOut_aux (instrs : inst list) (i : string) = 
        match instrs with
        | [] -> [Out(i,i)]
        | x :: subL -> (
            match x with
            | Move(_, src) when src = i ->
                (Out(i,i)) :: x :: subL
            | Oper(_,_,s1,s2) when s1 = i || s2 = i ->
                (Out(i,i)) :: x :: subL
            | Move(dst, _) when dst = i ->
                (Out(i,i)) :: x :: subL
            | Oper(_,dst,_,_) when dst = i ->
                (Out(i,i)) :: x :: subL
            | In(dst,_) when dst = i ->
                (Out(i,i)) :: x :: subL
            | _ -> 
                x :: (insertOut_aux subL i)
        )
    in

    if SSet.is_empty var_out then
       prog
    else
        let v = SSet.choose var_out in 
        let var_in = SSet.remove v var_out in 
            insertOut (insertOut_aux prog v) var_in

let pickWorst (t : SSet.t) (prog : inst list) = 
    let rec pick (s : string) k = function
    | [] -> k
    | i :: subL -> 
        (match i with
        | Move(dst,src) when dst = s || src = s -> k 
        | In(dst, _) when dst = s -> k
        | Out(src,_) when src = s -> k
        | Oper(_, dst, s1 , s2) when dst = s || s1 = s || s2 = s -> k
        | _ -> pick s (k + 1) subL)
    
    in let lp = SSet.fold (fun k l -> (k, pick k 0 prog) :: l) t [] 
    in let lp = List.sort (fun (_,x) (_,y) -> Int.compare y x) lp
    in let (k,v) = (List.hd lp) in 
    if v > 5 then
        Some k
    else 
        None

let rename (name : string) (reg : string) (sta : string) = 
    if name = reg then 
        sta
    else if name = sta then 
        reg
    else 
        name

let partial (i : inst) (reg : string) = 
    match i with 
    | Move(_,src) -> Move(reg, src)
    | In(_, v) -> In(reg, v)
    | Out(src,v) -> Out(src, v)
    | Oper(o, _, s1 , s2) -> Oper(o, reg, s1,s2)   

let spill (i : inst) (sta : string) (reg : string) = 
    match i with 
    | Move(dst,src) -> Move(rename dst reg sta, rename src reg sta)
    | In(dst, v) -> In(rename dst reg sta, v)
    | Out(src,v) -> Out(rename src reg sta, v)
    | Oper(o, dst, s1 , s2) -> Oper(o, rename dst reg sta, rename s1 reg sta, rename s2 reg sta)   

let rec spilling (prog : inst list) (accReg : SSet.t) (accStack : SSet.t) = 
    match prog with
    | [] -> []
    | i :: subL -> 
        (match defN i with
        | Some stack when SSet.mem stack accStack -> 
            (match pickWorst accReg prog with
            | None ->  (stackAlloc i accStack) @ (spilling subL accReg accStack)  
            | Some reg ->  
                let subL = List.map (fun i -> spill i stack reg) subL in 
                    if SSet.mem stack (useN i) then
                        Move("tmp", stack) :: 
                        Move(stack, reg) :: 
                        (stackAlloc (spill (partial i reg) stack "tmp") accStack) @ 
                        (spilling subL accReg accStack)
                    else
                        Move(stack, reg) :: (stackAlloc (partial i reg) accStack) @ (spilling subL accReg accStack)
            )
        | _ -> (stackAlloc i accStack) @ (spilling subL accReg accStack)  
    )

(* Construit un fichier pour jasmin *)
let build (max : int) (src : string) (dst : string) = 
    let (variables, instrs) = parse src in
    let instrs = (buildIn variables.var_in) @ instrs in
    let instrs = List.rev (insertOut (List.rev instrs) variables.var_out) in 
    List.iter (fun x -> Printf.eprintf "%s; \n" (pprintInst x)) instrs;
    let instrs = reduce instrs in 
    let (accReg, accStack), newProg = analyse instrs variables.var_out (max - 1) in
    let toSet acc = (List.fold_left (fun s x -> SSet.add x s) SSet.empty acc) in
    let newProg = spilling (List.rev newProg) (toSet accReg) (toSet accStack) in 
    let newProg = List.rev newProg in 
        slpToJasmin dst newProg accStack ("tmp" :: accReg) 

(* Construit un fichier C *)
let buildC (src : string) (dst : string) = 
    let header = "void f(long* A, long* B, long* C){ \n" in 
    let footer = "\n}\n" in 
    let translate (i : inst) = 
        match i with
        | Out(dst,_) -> (getIndiceFromName dst)  ^ " = " ^ dst ^ ";"
        | In(src,_) -> "long " ^ src ^ " = " ^ (getIndiceFromName src) ^ ";"
        | Move(dst, src) -> dst ^ " = " ^ src ^ ";"
        | Oper(Mul,dst,s1,s2) -> "long " ^ dst ^ " = " ^ s1 ^ " & " ^ s2 ^ ";"    
        | Oper(Sum,dst,s1,s2) -> "long " ^ dst ^ " = " ^ s1 ^ " ^ " ^ s2 ^ ";"  
    in 
    let (variables, instrs) = parse src in
    let declaration = SSet.fold (fun x str -> str ^ "\tlong " ^ x ^ ";\n") variables.var_out "" in 
    let definition = SSet.fold (fun x str -> str ^ "\tlong " ^ x ^ " = " ^ (getIndiceFromName x) ^ ";\n") variables.var_in "" in  
    let cinstrs = List.fold_right (fun x str -> str ^ "\t" ^ (translate x) ^ "\n") (List.rev instrs) "" in 
    let result = SSet.fold (fun x str -> str ^ "\t" ^ (getIndiceFromName x) ^ " = " ^ x ^ ";\n") variables.var_out "" in 
    let saveProg = open_out dst in 
        output_string saveProg (header ^ declaration ^ definition ^ cinstrs ^ result ^ footer)

let doubleMain src dstJasmin dstC = 
    build 14 src dstJasmin;
    buildC src dstC;
;;

let main () = 
    doubleMain "test/slp_630.txt" "test/slp_630.jazz" "test/slp_630.c";
    doubleMain "test/slp_big.txt" "test/slp_big.jazz" "test/slp_big.c"
;;

main ()