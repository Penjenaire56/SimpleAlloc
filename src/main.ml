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
    if (String.get n 0) = 'C' then {
        var_in = v.var_in;
        var_out = SSet.add n (v.var_out);
    } else if (String.get n 0) = 'A' || (String.get n 0) = 'B' then {
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
            | Oper(_, dst, _,_) | In(dst,_) -> 
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
    in aux instrs 100

(* Donne les variables définies pendant une instruction *)
let defN = function
    | Out(_) -> None
    | In(d,_) 
    | Move(d,_) 
    | Oper(_,d,_,_) -> Some(d)

(* Donne les variables utilisés pendant une instruction *)
let useN = function
    | In(_) -> SSet.empty
    | Out(s1,_)
    | Move(_, s1) -> SSet.singleton s1
    | Oper(_, _, s1, s2) -> SSet.add s1 (SSet.singleton s2) 

let popS (newest : SSet.t) = 
    let n = SSet.choose newest in 
        (n, SSet.remove n newest) 

let popGood (free : int IMap.t) = 
    let rec good lfree maxk maxv =
        match lfree with 
        | [] -> (maxk, maxv)
        | (k,v) :: subL when v > maxv -> 
            good subL k v
        | (_,_) :: subL ->
            good subL maxk maxv

    in good (IMap.bindings free) (-1) (-1)

let popBad (free : int IMap.t) =  
    let rec bad lfree mink minv =  
        match lfree with 
        | [] -> (mink, minv)
        | (k,v) :: subL when v < minv -> 
            bad subL k v
        | (_,_) :: subL ->
            bad subL mink minv

    in bad (IMap.bindings free) 100000 100000

let popI (free : int IMap.t) (usage : int IMap.t) heur =
    let (k,v) = if heur < 7 then
        popGood free
    else
        popBad free
    in
        (k, IMap.remove k free, IMap.add k v usage) 

let getHeur instrs n = 
    let rec heuristic n instrs acc =
        match instrs with 
        | [] -> 0
        | i :: subL -> (
            match i with 
            | Move(d,_) | Oper(_, d, _,_) | In(d,_) when d = n -> 
                acc
            | _ -> 
                heuristic n subL (acc + 1)
        ) in 
    heuristic n instrs 1

let getFresh free usage k instrs n = 
    let heur = (getHeur instrs n) in 
    if IMap.is_empty free then 
        (k + 1, free, IMap.add k 0 usage, k)
    else let (f,free, usage) = popI free usage heur in 
        (k, free, usage, f)

let rec aux_update newest k (regs : string IMap.t) (binds : int SMap.t) free usage instrs = 
    if SSet.is_empty newest then
        (k, free, usage, regs, binds)
    else 
        let (n, newest) = popS newest in 
        let (k, free, usage, f) = getFresh free usage k instrs n in 
            aux_update newest k (IMap.add f n regs) (SMap.add n f binds) free usage instrs

(* Mets à jour les binds et les variables définies *)
let update (regs : string IMap.t) (binds : int SMap.t) def newest free usage k instrs = 
    match def with
    | None -> aux_update newest k regs binds free usage instrs 
    | Some(def) -> 
        if (SSet.cardinal newest) > 0 then
            (* Reranger dans le free pour choisir le meilleur registre *)
            let n = (SMap.find def binds) in 
            let v = IMap.find n usage in 
                aux_update newest k regs binds (IMap.add n v free) (IMap.remove n usage) instrs
        else   
            let n = (SMap.find def binds) in 
            let v = IMap.find n usage in 
            (k, IMap.add n v free, IMap.remove n usage ,regs, binds)

let getRegistreName (var : string) (binds : int SMap.t) = 
    "r" ^ (string_of_int (SMap.find var binds))

let getRegistreNames (d,s1,s2) prevbinds binds = 
    let rdest = getRegistreName d prevbinds in 
    let rserc1 = getRegistreName s1 binds in 
    let rserc2 = getRegistreName s2 binds in 
        (rdest, rserc1, rserc2)

(* Construit les nouvelles instructions avec les registres générés automatiquement *)
let distribute (i : inst) binds (prevbinds : int SMap.t) (newest : SSet.t) regs k free usage instrs = 
    match i with
    | Move(dst, src) -> 
        [Move(getRegistreName dst prevbinds, getRegistreName src binds)]
    | In(src, name) -> 
        [In(getRegistreName src binds, name)]
    | Out(dst, name) -> 
        [Out(getRegistreName dst binds, name)]
    | Oper(op, d, s1, s2) when (SSet.mem s1 newest) && (SSet.mem s2 newest) -> 
        let tmp1 = "tmp1" in 
        let (_, _, _, _, binds) = aux_update (SSet.singleton tmp1) k regs binds free usage instrs in 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds binds in 
        let rtmp1 = getRegistreName tmp1 binds in 
        [
            Oper(op, rdest, rtmp1, rserc2);
            Move(rtmp1, rserc1);
        ]
    | Oper(op, d, s1, s2) when (SSet.mem s1 newest) -> 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds binds in 
        [
            Oper(op, rdest, rserc2, rserc1);
        ]
    | Oper(op, d, s1, s2) -> 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds binds in 
            [Oper(op, rdest, rserc1, rserc2)] 

let clean prog =
    List.filter (fun x -> 
        match x with 
        | Move(d,s) -> d <> s 
        | _ -> true
    ) prog 

let update_usage (usage : int IMap.t) (binds : int SMap.t) (vars : SSet.t) = 
    SSet.fold (fun s u -> 
        let k = (SMap.find s binds) in
        IMap.add k ((IMap.find k u) + 1) u
    ) vars usage

(* Construit un nouveau programme avec les registres automatiquement alloués *)
let analyse (instr : inst list) (initials : SSet.t) = 
    let instr = List.rev instr in 

    let free = IMap.empty in 

    let (k, regs, usage) = SSet.fold 
        (fun s (k,m,u) -> k + 1, (IMap.add k s m), (IMap.add k 0 u)) 
        initials (0, IMap.empty, IMap.empty) 
    in 

    let binds = IMap.fold (fun k v m -> SMap.add v k m) regs SMap.empty in

    let rec cross (variables : SSet.t) (r : string IMap.t) (bind : int SMap.t) (k : int) (free : int IMap.t) (usage : int IMap.t) = function
    | [] -> []
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

        let previousBind = bind in 
        let (k,free, usage, r,bind) = update r bind def newest free usage k subL in 

        let instrs = distribute i bind previousBind (SSet.diff use newest) r k free usage subL in 

        let usage = update_usage usage bind use in

        let subL = cross (SSet.union pre use) r bind k free usage subL in 
            instrs @ subL
    in 
    let prog = cross initials regs binds k free usage instr in 
    let optiProg = clean prog in optiProg

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


let findOccur (i : inst) = 
    match i with 
    | In(s,_) | Out(s,_) -> [s]
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
        | None -> IMap.add v [k] m
        | Some(l) -> IMap.add v (k::l) m)
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
        | Oper(o , dst, s1, s2) when List.mem s1 stack && List.mem s2 stack -> 
            [
                Oper(o, dst, "tmp", s2);
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
        | Oper(o , dst, s1, s2) when List.mem s1 stack -> 
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
            | Move(dst, _) when dst = i ->
                x :: (Out(i,i)) :: subL
            | Oper(_,dst,_,_) when dst = i ->
                x :: (Out(i,i)) :: subL
            | In(dst,_) when dst = i ->
                x :: (Out(i,i)) :: subL
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

let rec stackOpti (prog : inst list) stack reg = 
    match prog with
    | [] -> []
    | [x] -> [x]
    | x :: y :: subL -> (
        match x,y with
        |  Out(s2, v), Move(dst,s1) when dst = s2 -> 
            Out(s1,v) :: (stackOpti subL stack reg)
            
        | _,_ -> x :: (stackOpti (y :: subL) stack reg) 
    )

let build (k : int) (src : string) (dst : string) = 
    let (variables, instrs) = parse src in
    let instrs = (buildIn variables.var_in) @ instrs in
    let instrs = insertOut instrs variables.var_out in 
    let instrs = reduce instrs in 
    let newProg = analyse instrs variables.var_out in 
    let (accStack, accReg) = spilling newProg (k - 1) in  
    let newProg = stackAlloc newProg accStack in 
    let newProg = stackOpti newProg accStack accReg in 
        slpToJasmin dst newProg accStack ("tmp" :: accReg) 

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

    let newProg = analyse instrs variables.var_out in 
        saveProg "test/slp_630_alloc.txt" newProg;
    
    Printf.eprintf "Interprétation avec allocation \n"; 
    let env = setupEnv (SMap.empty) 
        [("r21", 1);("r24", 4);("r7", 5);("r12", 8);("r11", 9);("r8", 2);
        ("r18", 2);("r28", 3);("r29", 6);("r2", 2);("r17", 4);("r27", 0);] 

    in let exec = inter newProg env in 
        SMap.iter (fun x v -> Printf.eprintf "%s " (x ^ " = " ^ (string_of_int v))) (SMap.filter (fun x _ -> (getRegistreNumber x) < 6) exec);
        Printf.eprintf "\n"

let main () = 
    build 15 "test/slp_630.txt" "test/slp_630.jazz";
    buildC "test/slp_630.txt" "test/slp_630.c";
;;

main ()